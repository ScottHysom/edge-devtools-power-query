/* 
 * Selectors. Loads a performance profile JSON file that was exported by Edge DevTools and returns the 
 * CSS selectors found in the profile.
 */
let
    Selectors =
        let
            /*
             * Removes events that we don't care about. 
             */
            RemoveUnrelatedEvents = (sourceTable as table) as table =>
                Table.SelectRows(
                    sourceTable,
                    each
                        // Retain initial metadata...
                        [name] = "thread_name"
                        // Retain typical thread-starting events so we can calculate event start times...
                        or (
                            [cat] = "loading"
                            or [name] = "RunTask"
                            or [name] = "BeginFrame"
                            or [name] = "TracingSessionIdForWorker"
                            or [name] = "navigationStart"
                        )
                        // Retain these events...
                        // Selector Stats
                        or [name] = "SelectorStats"
                ),
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
             * Adds a key column that makes the row unique.
             */
            AddKeyColumn = (sourceTable as table) as table =>
                let
                    KeyAdded = Table.AddColumn(
                        sourceTable, "Key", each Text.Combine({Number.ToText([ts]), Number.ToText([tid])}, "-")
                    )
                in
                    KeyAdded,
            /*
             * Builds a table of events for the thread `threadId`.
             */
            GetEventsForThread = (sourceTable as table, threadId as number)/*as table*/ =>
                let
                    SelectorStatsEvents = Table.SelectRows(
                        sourceTable, each [tid] = threadId and [name] = "SelectorStats"
                    ),
                    KeyAdded = AddKeyColumn(SelectorStatsEvents)
                in
                    KeyAdded,
            /*
             * Returns a table of events for all the render threads.
             */
            GetRenderThreadEvents = (sourceTable as table, renderThreadIds as list) as table =>
                let
                    FinalState = List.Accumulate(
                        /* List */ renderThreadIds,
                        /* Seed */ [
                            Tables = {}
                        ],
                        /* Accumulator */ (
                            state, threadId
                        ) =>
                            let
                                ThreadTable = GetEventsForThread(sourceTable, threadId),
                                NewState = [
                                    Tables = List.Combine({state[Tables], {ThreadTable}})
                                ]
                            in
                                NewState
                    ),
                    TablesCombined = Table.Combine(FinalState[Tables])
                in
                    TablesCombined,
            /*
             * Expands the selector stats into columns.
             */
            ExpandSelectorTimings = (sourceTable as table) as table =>
                let
                    SelectorTimingsExpandedAsRecords = Table.ExpandListColumn(sourceTable, "selector_timings"),
                    SelectorTimingsExpanded = Table.ExpandRecordColumn(
                        SelectorTimingsExpandedAsRecords,
                        "selector_timings",
                        {
                            "elapsed (us)",
                            "fast_reject_count",
                            "match_attempts",
                            "match_count",
                            "selector",
                            "style_sheet_id"
                        },
                        {
                            "Elapsed (us)",
                            "Fast Reject Count",
                            "Match Attempts",
                            "Match Count",
                            "Selector",
                            "Style Sheet ID"
                        }
                    ),
                    SlowRejectCountAdded = Table.AddColumn(
                        SelectorTimingsExpanded,
                        "Slow Reject Count",
                        each [Match Attempts] - [Match Count] - [Fast Reject Count]
                    ),
                    RejectCountAdded = Table.AddColumn(
                        SlowRejectCountAdded, "Reject Count", each [Match Attempts] - [Match Count]
                    )
                in
                    RejectCountAdded,
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
                    ArgsExpanded = Table.ExpandRecordColumn(
                        sourceTable, "args", {"selector_stats"}, {"selector_stats"}
                    ),
                    SelectorStatsExpanded = Table.ExpandRecordColumn(
                        ArgsExpanded, "selector_stats", {"selector_timings"}, {"selector_timings"}
                    ),
                    SelectorTimingsExpanded = ExpandSelectorTimings(SelectorStatsExpanded)
                in
                    SelectorTimingsExpanded,
            /* 
             * Returns the configuration value of `parameterName` from the configuration table `configTableName`.
             */
            GetConfigValue = (
                configTableName as text,
                parameterName as text,
                optional defaultValue as any,
                optional coerceType as type
            ) =>
                let
                    GetParameterValue = (
                        row as record, parameterName as text, defaultValue as any, parameterType as type
                    ) =>
                        let
                            TextValue = row[Value],
                            RawValue = Value.FromText(TextValue),
                            CoercedValue =
                                try
                                    Value.As(RawValue, parameterType) otherwise Value.As(defaultValue, parameterType),
                            TrimmedValue =
                                if Type.Is(parameterType, type text) then
                                    Text.Trim(CoercedValue)
                                else
                                    CoercedValue
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
            UnrelatedEventsRemoved = RemoveUnrelatedEvents(BasicTraceEvents),
            // Filter to just the events on the render threads
            RenderThreadIds = GetRenderThreadIds(UnrelatedEventsRemoved),
            RenderThreadEvents = GetRenderThreadEvents(UnrelatedEventsRemoved, RenderThreadIds),
            // Expand columns that have records
            ArgsExpanded = ExpandArgs(RenderThreadEvents),
            // Group by Selector
            GroupedBySelectors = Table.Group(
                ArgsExpanded,
                "Selector",
                {
                    {"Elapsed (us)", each List.Sum([#"Elapsed (us)"])},
                    {"Match Attempts", each List.Sum([Match Attempts])},
                    {"Match Count", each List.Sum([Match Count])},
                    {"Reject Count", each List.Sum([Reject Count])},
                    {"Fast Reject Count", each List.Sum([Fast Reject Count])},
                    {"Slow Reject Count", each List.Sum([Slow Reject Count])},
                    {"Selector Count", each List.Count([Selector])}
                }
            ),
            // Remove, re-order and assign types to columns
            ColumnsRemoved = Table.RemoveColumns(GroupedBySelectors, {"metadata"}),
            ColumnsRenamed = Table.RenameColumns(ColumnsRemoved, {{"tid", "Thread ID"}}),
            ColumnsReordered = Table.ReorderColumns(
                ColumnsRenamed,
                {
                    "Thread ID",
                    "Start Time (ms)",
                    "Activity",
                    "Recalc Style Iteration",
                    "Elapsed (us)",
                    "Match Attempts",
                    "Match Count",
                    "Reject Count",
                    "Fast Reject Count",
                    "Slow Reject Count",
                    "Selector",
                    "Style Sheet ID",
                    "dur",
                    "tdur",
                    "ph",
                    "pid",
                    "s",
                    "ts",
                    "tts",
                    "name",
                    "cat",
                    "Key",
                    "Iteration Key"
                }
            ),
            TypesChanged = Table.TransformColumnTypes(
                ColumnsReordered,
                {
                    {"Thread ID", Int64.Type},
                    {"Start Time (ms)", type number},
                    {"Activity", type text},
                    {"Recalc Style Iteration", type number},
                    {"Elapsed (us)", Int64.Type},
                    {"Match Attempts", type number},
                    {"Match Count", type number},
                    {"Reject Count", type number},
                    {"Fast Reject Count", type number},
                    {"Slow Reject Count", type number},
                    {"Selector", type text},
                    {"Style Sheet ID", type text},
                    {"dur", Int64.Type},
                    {"tdur", Int64.Type},
                    {"ph", type text},
                    {"pid", Int64.Type},
                    {"s", type text},
                    {"ts", Int64.Type},
                    {"tts", Int64.Type},
                    {"name", type text},
                    {"cat", type text},
                    {"Key", type text},
                    {"Iteration Key", type text}
                }
            ),
            ColumnsSorted = Table.Sort(
                TypesChanged,
                {"Thread ID", "ts", {"Reject Count", Order.Descending}, {"Slow Reject Count", Order.Descending}}
            ),
            FinalColumns = ColumnsSorted
        in
            // FinalColumns
            GroupedBySelectors
in
    Selectors
