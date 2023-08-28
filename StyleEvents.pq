/* 
 * StyleEvents. Loads a performance profile JSON file that was exported by Edge DevTools and returns the style 
 * events found in the profile.
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
            ResolveState = (state as record, currentName as text) =>
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
     * Removes events that we don't care about.
     */
    RemoveUnrelatedEvents = (sourceTable as table) as table =>
        let
            CategoriesRemoved = Table.SelectRows(
                sourceTable,
                each
                    // Retain these events...
                    // Selector Stats
                    ([cat] = "disabled-by-default-blink.debug" and [name] = "SelectorStats")
                    // Schedule Style Recalculation
                    or ([cat] = "disabled-by-default-devtools.timeline" and [name] = "ScheduleStyleRecalculation")
                    // Layout, prepaint
                    or ([cat] = "devtools.timeline" and ([name] = "Layout" or [name] = "ParseHTML"))
                    // 'UpdateLayoutTree' is the "Recalculate Style" activity with the Total Time value
                    or ([cat] = "blink,devtools.timeline" and [name] = "UpdateLayoutTree")
                    // Custom perf markers
                    or [cat] = "blink.user_timing"
                    // Discard these events...
                    or (
                        // Profile metadata (threads, processes)
                        [cat] <> "__metadata"
                        // Loading events
                        and [cat] <> "loading"
                        // V8 events
                        and [cat] <> "v8"
                        and [cat] <> "v8.execute"
                        and [cat] <> "v8,devtools.timeline"
                        and [cat] <> "devtools.timeline,v8"
                        and [cat] <> "devtools.timeline,disabled-by-default-v8.gc"
                        and [cat] <> "disabled-by-default-v8.compile"
                        and [cat] <> "disabled-by-default-v8.cpu_profiler"
                        // Timers, function calls, event dispatching, layout, prepaint
                        and [cat] <> "devtools.timeline"
                        // Paint
                        and [cat] <> "devtools.timeline,rail"
                        // RunTask, InvalidatedLayout, ScheduleStyleRecalculation
                        and [cat] <> "disabled-by-default-devtools.timeline"
                        // EventDispatcher, LocalFrameView, SelectorStats, UpdateCounters, UpdateLayer
                        and [cat] <> "disabled-by-default-blink.debug"
                        // Frame-related
                        and [cat] <> "disabled-by-default-devtools.timeline.frame"
                        // Intersection observers, HitTest, UpdateLayoutTree
                        and [cat] <> "blink,devtools.timeline"
                        // Invalidation tracking includes style events, but VERY noisy
                        and [cat] <> "disabled-by-default-devtools.timeline.invalidationTracking"
                        // Screen snapshots
                        and [cat] <> "disabled-by-default-cc.debug.display_items,disabled-by-default-cc.debug.picture,disabled-by-default-devtools.timeline.picture"
                        // Animations
                        and [cat] <> "blink.animations,devtools.timeline,benchmark,rail"
                        // and [cat] <> "input,benchmark,devtools.timeline,latencyInfo"
                        // and [cat] <> "cc,disabled-by-default-devtools.timeline"
                    )
            )
        in
            CategoriesRemoved,
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
     * Adds an 'Activity' column, which maps event names to friendlier Dev Tools UI equivalent activities
     */
    AddActivityColumn = (sourceTable as table) as table =>
        let
            ColumnAdded = Table.AddColumn(
                sourceTable,
                "Activity",
                each
                    if [name] = "ScheduleStyleRecalculation" then
                        "Schedule Style Recalculation"
                    else if [name] = "UpdateLayoutTree" then
                        "Recalculate Style"
                    else if [cat] = "blink.user_timing" then
                        Text.Combine({"Perf Marker:", [name]})
                    else
                        [name]
            )
        in
            ColumnAdded,
    /*
     * Expands the selector stats into columns.
     */
    ExpandSelectorStats = (sourceTable as table) as table =>
        let
            SelectorTimingsExpandedAsRecords = Table.ExpandListColumn(sourceTable, "selector_timings"),
            SelectorTimingsExpanded = Table.ExpandRecordColumn(
                SelectorTimingsExpandedAsRecords,
                "selector_timings",
                {"elapsed (us)", "fast_reject_count", "match_attempts", "match_count", "selector", "style_sheet_id"},
                {"Elapsed", "Fast Reject Count", "Match Attempts", "Match Count", "Selector", "Style Sheet ID"}
            ),
            SlowRejectCountAdded = Table.AddColumn(
                SelectorTimingsExpanded, "Slow Reject Count", each [Match Attempts] - [Fast Reject Count]
            )
        in
            SlowRejectCountAdded,
    /*
     * Returns a dictionary (as a record) whose field names are keys to the records in `records`.
     */
    ToDictionary = (records as list, key as text) as record =>
        List.Accumulate(
            records,
            [],
            (accumRecord, current) => Record.AddField(accumRecord, Text.From(Record.Field(current, key)), current)
        ),
    /*
     * Returns a record as a dictionary, whose property names are keys (using the timestamp) to nested aggregate 
     * records. The aggregate record contains the selector stats in aggregate for the selector stat that occurred at 
     * that timestamp.
     */
    BuildSelectorStatsAggregates = (sourceTable as table) as record =>
        let
            SelectorStatsRows = Table.SelectRows(sourceTable, each [name] = "SelectorStats"),
            SelectorStatsExpanded = ExpandSelectorStats(SelectorStatsRows),
            SelectorStatsAggregatedByTimestamp = Table.Group(
                SelectorStatsExpanded,
                "ts",
                {
                    {"Elapsed (us)", each List.Sum([Elapsed])},
                    {"Fast Reject Count", each List.Sum([Fast Reject Count])},
                    {"Slow Reject Count", each List.Sum([Slow Reject Count])},
                    {"Match Attempts", each List.Sum([Match Attempts])},
                    {"Match Count", each List.Sum([Match Count])},
                    {"Selector Count", each List.Count([Selector])}
                }
            ),
            SelectorStatsRecords = Table.ToRecords(SelectorStatsAggregatedByTimestamp),
            SelectorStatsDictionary = ToDictionary(SelectorStatsRecords, "ts")
        in
            SelectorStatsDictionary,
    /*
     * Adds columns related to selector stats.
     */
    AddSelectorStatsColumns = (sourceTable as table) as table =>
        let
            SelectorStatsDictionary = BuildSelectorStatsAggregates(sourceTable),
            ElapsedTotalAdded = Table.AddColumn(
                sourceTable,
                "Elapsed (us)",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[#"Elapsed (us)"]
                    else
                        null
            ),
            FastRejectCountAdded = Table.AddColumn(
                ElapsedTotalAdded,
                "Fast Reject Count",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[Fast Reject Count]
                    else
                        null
            ),
            SlowRejectCountAdded = Table.AddColumn(
                FastRejectCountAdded,
                "Slow Reject Count",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[Slow Reject Count]
                    else
                        null
            ),
            MatchAttemptsAdded = Table.AddColumn(
                SlowRejectCountAdded,
                "Match Attempts",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[Match Attempts]
                    else
                        null
            ),
            MatchCountAdded = Table.AddColumn(
                MatchAttemptsAdded,
                "Match Count",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[Match Count]
                    else
                        null
            ),
            SelectorCountAdded = Table.AddColumn(
                MatchCountAdded,
                "Selector Count",
                each
                    if [name] = "SelectorStats" then
                        Record.Field(SelectorStatsDictionary, Number.ToText([ts]))[Selector Count]
                    else
                        null
            )
        in
            SelectorCountAdded,
    /*
     * Expands the `args` record into the desired set of columns.
     */
    ExpandArgs = (sourceTable as table) as table =>
        let
            ArgsExpanded = Table.ExpandRecordColumn(
                sourceTable,
                "args",
                {
                // `UpdateLayoutTree` properties
                "elementCount",
                // `StyleRecalcInvalidationTracking` properties
                "data",
                // `SelectorStats` properties
                "selector_stats"},
                {"Element Count", "data", "selector_stats"}
            ),
            /* The `UpdateLayoutTree` event schema:
                {
                    "args": {
                        "beginData": {
                            "frame":"<text>"            // <--- we don't use
                        },
                        "elementCount":<number>         // <--- we want this
                    },
                    "cat":"blink,devtools.timeline",
                    "dur":<number>,
                    "name":"UpdateLayoutTree",
                    "ph":"<text>",
                    "pid":<number>,
                    "tdur":<number>,
                    "tid":<number>,
                    "ts":<number>,
                    "tts":<number>
                }
            */
            /* The `StyleRecalcInvalidationTracking` event schema:
                {
                    "args": {
                        "data": {
                            "extraData": "<text>",
                            "frame":"<text>",           // <--- we don't use
                            "invalidationList": [
                                { 
                                    "classes": ["<text>", ...],
                                    "id": "<text>"
                                },
                                ...
                            ],
                            "nodeId": <number>,
                            "nodeName": "<text>",
                            "reason": "<text>",
                            "subtree": <logical>
                        }
                    },
                    "cat": "disabled-by-default-devtools.timeline.invalidationTracking",
                    "name": "StyleInvalidatorInvalidationTracking",
                    "ph": "<text>",
                    "pid": <number>,
                    "s": "<text>",
                    "tid": <number>,
                    "ts": <number>,
                    "tts": <number>
                }
            */
            DataExpanded = Table.ExpandRecordColumn(
                ArgsExpanded,
                "data",
                {"extraData", "nodeId", "nodeName", "reason", "subtree"},
                {"Extra Data", "Node Id", "Node Name", "Reason", "Subtree"}
            ),
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
            SelectorStatsExpanded = Table.ExpandRecordColumn(
                DataExpanded, "selector_stats", {"selector_timings"}, {"selector_timings"}
            )
        in
            SelectorStatsExpanded,
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
    UnrelatedEventsRemoved = RemoveUnrelatedEvents(RenderThreadEvents),
    // Add pre-expansion columns
    ActivityAdded = AddActivityColumn(UnrelatedEventsRemoved),
    StartTimeAdded = Table.AddColumn(ActivityAdded, "Start Time (ms)", each ([ts] - ThreadStartTimestamp) / 1000),
    TotalTimeColumnAdded = Table.AddColumn(
        StartTimeAdded, "Total Time (ms)", each if [dur] <> null then [dur] / 1000 else null
    ),
    // Expand columns that have records
    ArgsExpanded = ExpandArgs(TotalTimeColumnAdded),
    // Add post-expansion columns
    SelectorStatsAdded = AddSelectorStatsColumns(ArgsExpanded),
    // Remove, re-order and assign types to columns
    ColumnsRemoved = Table.RemoveColumns(SelectorStatsAdded, {"metadata", "selector_timings"}),
    ColumnsReordered = Table.ReorderColumns(
        ColumnsRemoved,
        {
            "Start Time (ms)",
            "Activity",
            "Total Time (ms)",
            "Element Count",
            "Elapsed (us)",
            "Fast Reject Count",
            "Slow Reject Count",
            "Match Attempts",
            "Match Count",
            "Selector Count",
            "Extra Data",
            "Node Id",
            "Node Name",
            "Reason",
            "Subtree",
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
            {"Total Time (ms)", type number},
            {"Element Count", type number},
            {"Elapsed (us)", type number},
            {"Fast Reject Count", type number},
            {"Slow Reject Count", type number},
            {"Match Attempts", type number},
            {"Match Count", type number},
            {"Selector Count", type number},
            {"Extra Data", type any},
            {"Node Id", Int64.Type},
            {"Node Name", type text},
            {"Reason", type text},
            {"Subtree", type logical},
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
    ColumnsSorted = Table.Sort(TypesChanged, {"ts"}),
    FinalColumns = ColumnsSorted
in
    FinalColumns
