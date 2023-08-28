/* 
 * SelectorStats. Loads a performance profile JSON file that was exported by Edge DevTools and returns the 
 * "selector_stats" found in the profile.
 */
let
    /*
     * Selects a single row in `sourceTable` that has an event name that matches one of `eventNames`, which might not 
     * necessarily be the first row in the table that meets the condition. 
     *
     * We can't use Table.FirstN because that function stops the moment a row doesn't meet the condition, which means 
     * it can return no rows, even if a row further down does meet the condition.
     */
    SelectSingleRowAsRecord = (sourceTable as table, eventNames as list) as record =>
        let
            // Avoid Table.SelectRows with expensive conditions (like List.Contains) because `sourceTable` can be O(5).
            // Instead, accumulate based on the shorter `eventNames` list and succeed out early when the first row
            // is found.
            InitialState = [SingleRecord = null, FoundFirstRow = false],
            ResolveState = (state as record, currentName as text) as record =>
                let
                    MatchedRows = Table.SelectRows(sourceTable, each [name] = currentName),
                    FoundRow = Table.RowCount(MatchedRows) > 0,
                    SingleRecord = if FoundRow then Table.First(MatchedRows) else state[SingleRecord]
                in
                    [SingleRecord = SingleRecord, FoundFirstRow = FoundRow],
            FinalState = List.Accumulate(
                eventNames,
                InitialState,
                (state as record, currentName as text) =>
                    if state[FoundFirstRow] then
                        state
                    else
                        ResolveState(state, currentName)
            )
        in
            FinalState[SingleRecord],
    /*
     * Expands the `sourceTable` so the basic trace event fields are available.
     */
    GetBasicTraceEvents = (sourceTable as table) as table =>
        let
            TraceEventsAsRecords = Table.ExpandListColumn(sourceTable, "traceEvents"),
            /* Basic `traceEvents` schema:
                {
                    "args": { ... },
                    "cat":"<text>",
                    "dur":<number>,
                    "name":"<text>",
                    "ph":"<text>",
                    "pid":<number>,
                    "tdur":<number>,
                    "s": "<text>",
                    "tid":<number>,
                    "ts":<number>,
                    "tts":<number>
                }
            */
            TraceEventsExpanded = Table.ExpandRecordColumn(
                TraceEventsAsRecords,
                "traceEvents",
                {"args", "cat", "dur", "name", "ph", "pid", "tdur", "s", "tid", "ts", "tts"}
            ),
            MetadataRowsRemoved = Table.SelectRows(TraceEventsExpanded, each [cat] <> "__metadata")
        in
            MetadataRowsRemoved,
    /*
     * Determines the main render thread ID based on the first 'StyleRecalcInvalidationTracking' event.
     */
    GetMainRenderThreadId = (sourceTable as table) as number =>
        let
            MainRenderThreadRecord = SelectSingleRowAsRecord(sourceTable, {"StyleRecalcInvalidationTracking"}),
            MainRenderThreadId =
                if MainRenderThreadRecord <> null then
                    MainRenderThreadRecord[tid]
                else
                    error "No main render thread found"
        in
            MainRenderThreadId,
    /*
     * Gets the starting timestamp.
     */
    GetStartTimestamp = (sourceTable as table) as number =>
        let
            FirstRecord = Table.First(sourceTable),
            StartTimestamp =
                if FirstRecord[ts] <> 0 then
                    FirstRecord[ts]
                else
                    error "The first event does not have a timestamp"
        in
            StartTimestamp,
    /*
     * Expands the `args` record into the desired set of columns.
     */
    ExpandArgs = (sourceTable as table) as table =>
        let
            /* The `SelectorStats` event schema:
                {
                    "args": {
                        "selector_stats": {
                            "selector_timings": [
                                {
                                    "elapsed (us)": <number>,
                                    "fast_reject_count": <number>,
                                    "match_attempts": <number>,
                                    "match_count": <number>,
                                    "selector": "<text>",
                                    "style_sheet_id":"<text>"
                                },
                                ...
                            ]
                        }
                    },
                    "cat": "disabled-by-default-blink.debug",
                    "dur": <number>,
                    "name": "SelectorStats",
                    "ph": "<text>",
                    "pid": <number>,
                    "tdur": <number>,
                    "tid": <number>,
                    "ts": <number>,
                    "tts": <number>
                }
            */
            ArgsExpanded = Table.ExpandRecordColumn(sourceTable, "args", {"selector_stats"}, {"selector_stats"}),
            SelectorStatsExpanded = Table.ExpandRecordColumn(
                ArgsExpanded, "selector_stats", {"selector_timings"}, {"selector_timings"}
            ),
            SelectorTimingsExpandedAsRecords = Table.ExpandListColumn(SelectorStatsExpanded, "selector_timings"),
            SelectorTimingsExpanded = Table.ExpandRecordColumn(
                SelectorTimingsExpandedAsRecords,
                "selector_timings",
                {"elapsed (us)", "fast_reject_count", "match_attempts", "match_count", "selector", "style_sheet_id"},
                {"Elapsed (us)", "Fast Reject Count", "Match Attempts", "Match Count", "Selector", "Style Sheet ID"}
            )
        in
            SelectorTimingsExpanded,
    /* 
     * Returns the configuration value of `parameterName` from the configuration table `configTableName`.
     */
    GetConfigValue = (
        configTableName as text, parameterName as text, optional defaultValue as any, optional coerceType as type
    ) =>
        let
            GetParameterValue = (row as record, parameterName as text, defaultValue as any, parameterType as type) =>
                let
                    TextValue = row[Value],
                    RawValue = Value.FromText(TextValue),
                    CoercedValue =
                        try Value.As(RawValue, parameterType) otherwise Value.As(defaultValue, parameterType),
                    TrimmedValue = if Type.Is(parameterType, type text) then Text.Trim(CoercedValue) else CoercedValue
                in
                    TrimmedValue,
            // ------------
            Type = if coerceType <> null then coerceType else type text,
            HasDefaultValue = defaultValue <> null,
            ConfigTable = Excel.CurrentWorkbook(){[Name = configTableName]}[Content],
            ParameterRows = Table.SelectRows(ConfigTable, each [Parameter] = parameterName),
            Value =
                if Table.RowCount(ParameterRows) > 0 then
                    GetParameterValue(ParameterRows{0}, parameterName, defaultValue, Type)
                else if HasDefaultValue then
                    defaultValue
                else
                    error
                        "Configuration parameter '"
                            & parameterName
                            & "' not found in the '"
                            & configTableName
                            & "' table."
        in
            Value,
    /*
     * Remove any surrounding quotes from the config value
     */
    GetFilePathConfigValue = (configTableName as text, parameterName as text) =>
        let
            TextWithMaybeQuotes = GetConfigValue(configTableName, parameterName),
            // Remove any surrounding quotes
            RemoveQuotes = Text.Remove(TextWithMaybeQuotes, {""""})
        in
            RemoveQuotes,
    /*
     ***************************************************************************************
     * Main
     ***************************************************************************************
     */
    // Parameters to use
    InputFilePath = GetFilePathConfigValue("Config", "InputFilePath"),
    // Read and transform file
    SourceAsRecords = Json.Document(File.Contents(InputFilePath)),
    SourceAsTable = Table.FromRecords({SourceAsRecords}),
    BasicTraceEvents = GetBasicTraceEvents(SourceAsTable),
    // Filter to just the events on the render threads
    MainRenderThreadId = GetMainRenderThreadId(BasicTraceEvents),
    RenderThreadEvents = Table.SelectRows(BasicTraceEvents, each [tid] = MainRenderThreadId),
    ThreadStartTimestamp = GetStartTimestamp(RenderThreadEvents),
    // Filter to just the SelectorStats events
    SelectorStatsEvents = Table.SelectRows(BasicTraceEvents, each [name] = "SelectorStats"),
    // Add pre-expansion columns
    ActivityAdded = Table.AddColumn(
        SelectorStatsEvents, "Activity", each if [name] = "SelectorStats" then "Selector Stats" else [name]
    ),
    RecalcStyleIterationAdded = Table.AddIndexColumn(ActivityAdded, "Recalc Style Iteration", 1),
    StartTimeAdded = Table.AddColumn(
        RecalcStyleIterationAdded, "Start Time (ms)", each ([ts] - ThreadStartTimestamp) / 1000
    ),
    // Expand columns that have records
    ArgsExpanded = ExpandArgs(StartTimeAdded),
    // Add post-expansion columns
    SlowRejectCountAdded = Table.AddColumn(
        ArgsExpanded, "Slow Reject Count", each [Match Attempts] - [Fast Reject Count]
    ),
    // Remove, re-order and assign types to columns
    ColumnsRemoved = Table.RemoveColumns(SlowRejectCountAdded, {"metadata"}),
    ColumnsReordered = Table.ReorderColumns(
        ColumnsRemoved,
        {
            "Start Time (ms)",
            "Activity",
            "Recalc Style Iteration",
            "Elapsed (us)",
            "Fast Reject Count",
            "Slow Reject Count",
            "Match Attempts",
            "Match Count",
            "Selector",
            "Style Sheet ID",
            "dur",
            "tdur",
            "ph",
            "pid",
            "tid",
            "s",
            "ts",
            "tts",
            "name",
            "cat"
        }
    ),
    TypesChanged = Table.TransformColumnTypes(
        ColumnsReordered,
        {
            {"Start Time (ms)", type number},
            {"Activity", type text},
            {"Recalc Style Iteration", type number},
            {"Elapsed (us)", Int64.Type},
            {"Fast Reject Count", type number},
            {"Slow Reject Count", type number},
            {"Match Attempts", type number},
            {"Match Count", type number},
            {"Selector", type text},
            {"Style Sheet ID", type text},
            {"dur", Int64.Type},
            {"tdur", Int64.Type},
            {"ph", type text},
            {"pid", Int64.Type},
            {"tid", Int64.Type},
            {"s", type text},
            {"ts", Int64.Type},
            {"tts", Int64.Type},
            {"name", type text},
            {"cat", type text}
        }
    ),
    ColumnsSorted = Table.Sort(TypesChanged, {"Recalc Style Iteration", {"Slow Reject Count", Order.Descending}}),
    FinalColumns = ColumnsSorted
in
    FinalColumns
