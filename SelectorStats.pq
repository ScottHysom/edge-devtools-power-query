/* 
 * SelectorStats. Loads a performance profile JSON file that was exported by Edge DevTools and returns the 
 * "selector_stats" found in the profile.
 */
let
    /*
     * Performs a binary search on `sortedList`, returning the index of `findValue` or null if not found.
     *
     * Callers should use this instead of List.Contains for very large lists because List.Contains performs 
     * a linear search.
     *
     * This function uses List.Generate instead of a recursive approach because Power Query does not perform
     * tail call optimization which can result in stack overflows.
     */
    BinarySearch = (sortedList as list, findValue as number) as nullable number =>
        let
            MinIndex = 0,
            MaxIndex = List.Count(sortedList) - 1,
            Result = List.Generate(
                /* Initial */ () =>
                    [
                        Iterations = 0,
                        FindValue = findValue,
                        MinIndex = MinIndex,
                        MaxIndex = MaxIndex,
                        MidIndex = Number.IntegerDivide(MinIndex + MaxIndex, 2)
                    ],
                /* Condition */ each
                    [MinIndex] <= [MaxIndex],
                /* Next */ each
                    if sortedList{[MidIndex]} < findValue then
                        [
                            Iterations = [Iterations] + 1,
                            MinIndex = [MidIndex] + 1,
                            MaxIndex = [MaxIndex],
                            MidIndex = Number.IntegerDivide([MidIndex] + 1 + [MaxIndex], 2),
                            FindValue = findValue
                        ]
                    else if sortedList{[MidIndex]} > findValue then
                        [
                            Iterations = [Iterations] + 1,
                            MinIndex = [MinIndex],
                            MaxIndex = [MidIndex] - 1,
                            MidIndex = Number.IntegerDivide([MinIndex] + [MidIndex] - 1, 2),
                            FindValue = findValue
                        ]
                    else
                        [
                            Iterations = [Iterations],
                            MinIndex = [MidIndex] + 1,
                            MaxIndex = [MidIndex] - 1,
                            MidIndex = [MidIndex],
                            FindValue = findValue
                        ],
                /* Selector */ each
                    [
                        Iterations = [Iterations],
                        MinIndex = [MinIndex],
                        MaxIndex = [MaxIndex],
                        MidIndex = [MidIndex],
                        FindValue = [FindValue],
                    ]
            ),
            LastRecord = List.Last(Result, null),
            FoundIndex = try if sortedList{LastRecord[MidIndex]} = findValue then LastRecord[MidIndex] else null otherwise null
        in
            FoundIndex,
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
            )
        in
            TraceEventsExpanded,
    /*
     * Returns a list of render thread IDs.
     */
    GetRenderThreadIds = (sourceTable as table) as list =>
        let
            ThreadRows = Table.SelectRows(sourceTable, each [name] = "thread_name"),
            WithThreadName = Table.ExpandRecordColumn(ThreadRows, "args", {"name"}, {"ThreadName"}),
            RenderThreadRows = Table.SelectRows(WithThreadName, each [ThreadName] = "CrRendererMain"),
            ThreadIds = List.Sort(RenderThreadRows[tid])
        in
            ThreadIds,
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
     * Adds a 'Recalc Style Iteration' column, which increments only for the Recalc Style events.
     */
    AddRecalcStyleIterationColumn = (sourceTable as table) =>
        let
            ColumnNames = Table.ColumnNames(sourceTable),
            EventNames = sourceTable[name],
            NewColumn = List.Generate(
                () => [RecalcStyleIteration = if EventNames{0} = "UpdateLayoutTree" then 1 else 0, i = 0],
                each [i] < Table.RowCount(sourceTable),
                each
                    [
                        RecalcStyleIteration = if EventNames{[i] + 1} = "UpdateLayoutTree" then
                            [RecalcStyleIteration] + 1
                        else
                            [RecalcStyleIteration],
                        i = [i] + 1
                    ],
                each [RecalcStyleIteration]
            ),
            FinalTable = Table.FromColumns(
                Table.ToColumns(sourceTable) & {NewColumn}, ColumnNames & {"Recalc Style Iteration"}
            )
        in
            FinalTable,
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
    TableBuffered = Table.Buffer(SourceAsTable),
    BasicTraceEvents = GetBasicTraceEvents(TableBuffered),
    // Filter to just the events on the render threads
    RenderThreadIds = GetRenderThreadIds(BasicTraceEvents),
    RenderThreadEvents = Table.SelectRows(BasicTraceEvents, each BinarySearch(RenderThreadIds, [tid]) <> null),
    ThreadStartTimestamp = GetStartTimestamp(RenderThreadEvents),
    // Filter to just the Recalc Styles and SelectorStats events. We will eventually reduce this to just SelectorStats
    // but we need the Recalc Styles to calculate the Recalc Style Iteration column.
    InitialEvents = Table.SelectRows(BasicTraceEvents, each [name] = "UpdateLayoutTree" or [name] = "SelectorStats"),
    RecalcStyleIterationAdded = AddRecalcStyleIterationColumn(InitialEvents),
    SelectorStatsEvents = Table.SelectRows(RecalcStyleIterationAdded, each [name] = "SelectorStats"),
    // Add pre-expansion columns
    ActivityAdded = Table.AddColumn(SelectorStatsEvents, "Activity", each [name]),
    StartTimeAdded = Table.AddColumn(ActivityAdded, "Start Time (ms)", each ([ts] - ThreadStartTimestamp) / 1000),
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
