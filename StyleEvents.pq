/* 
 * StyleEvents. Loads a performance profile JSON file that was exported by Edge DevTools and returns the style 
 * events found in the profile.
 */
let
    StyleEvents =
        let
            /*
             * Removes events that we don't care about. `filter` can be "keep-more" or "keep-less".
             */
            RemoveUnrelatedEvents = (sourceTable as table, filter as text) as table =>
                let
                    FilterLevel =
                        if filter = "keep-more" then
                            "keep-more"
                        else if filter = "keep-less" then
                            "keep-less"
                        else
                            error "filter argument is invalid",
                    Rows = Table.SelectRows(
                        sourceTable,
                        each
                            // Retain initial metadata...
                            (FilterLevel = "keep-more" and [name] = "thread_name")
                            // Retain typical thread-starting events so we can calculate event start times...
                            or (
                                FilterLevel = "keep-more"
                                and (
                                    [cat] = "loading"
                                    or [name] = "RunTask"
                                    or [name] = "BeginFrame"
                                    or [name] = "TracingSessionIdForWorker"
                                    or [name] = "navigationStart"
                                )
                            )
                            // Retain these events...
                            // Selector Stats
                            or [name] = "SelectorStats"
                            // 'UpdateLayoutTree' is the "Recalculate Style" activity with the Total Time value
                            or [name] = "UpdateLayoutTree"
                            // Schedule Style Recalculation
                            or [name] = "ScheduleStyleRecalculation"
                            // Layout, prepaint
                            or ([name] = "Layout" or [name] = "LayoutShift" or [name] = "ParseHTML")
                            // Custom perf markers
                            or [cat] = "blink.user_timing"
                    )
                in
                    Rows,
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
             * Adds a 'Recalc Style Iteration' column, which increments only for the Recalc Style events.
             */
            AddRecalcStyleIterationColumn = (sourceTable as table) =>
                let
                    SourceBuffered = Table.Buffer(sourceTable),
                    ColumnNames = Table.ColumnNames(SourceBuffered),
                    EventNames = SourceBuffered[name],
                    RowCount = Table.RowCount(SourceBuffered),
                    IterationColumn = List.Generate(
                        () => [RecalcStyleIteration = if EventNames{0} = "UpdateLayoutTree" then 1 else 0, RowIndex = 0],
                        each [RowIndex] < RowCount,
                        each
                            [
                                RecalcStyleIteration = if EventNames{[RowIndex] + 1} = "UpdateLayoutTree" then
                                    [RecalcStyleIteration] + 1
                                else
                                    [RecalcStyleIteration],
                                RowIndex = [RowIndex] + 1
                            ],
                        each [RecalcStyleIteration]
                    ),
                    FinalTable = Table.FromColumns(
                        Table.ToColumns(SourceBuffered) & {IterationColumn}, ColumnNames & {"Recalc Style Iteration"}
                    )
                in
                    FinalTable,
            /*
             * Adds an 'Activity' column, which maps event names to friendlier Dev Tools UI equivalent activities
             */
            AddActivityColumn = (sourceTable as table) as table =>
                let
                    ColumnAdded = Table.AddColumn(
                        sourceTable,
                        "Activity",
                        each
                            if [name] = "UpdateLayoutTree" then
                                "RecalculateStyle"
                            else if [cat] = "blink.user_timing" then
                                Text.Combine({"Perf Marker: ", [name]})
                            else
                                [name]
                    )
                in
                    ColumnAdded,
            /*
             * Adds a key column that makes the row unique.
             */
            AddKeyColumns = (sourceTable as table) as table =>
                let
                    KeyAdded = Table.AddColumn(
                        sourceTable, "Key", each Text.Combine({Number.ToText([ts]), Number.ToText([tid])}, "-")
                    ),
                    IterationKeyAdded = Table.AddColumn(
                        KeyAdded,
                        "Iteration Key",
                        each Text.Combine({Number.ToText([tid]), Number.ToText([Recalc Style Iteration])}, "-")
                    )
                in
                    IterationKeyAdded,
            /*
             * Builds a table of events for the thread `threadId`.
             */
            GetEventsForThread = (sourceTable as table, threadId as number)/*as table*/ =>
                let
                    ThreadEvents = Table.SelectRows(sourceTable, each [tid] = threadId and [name] <> "thread_name"),
                    ThreadStartTimestamp = GetStartTimestamp(ThreadEvents),
                    ScopedThreadEvents = RemoveUnrelatedEvents(ThreadEvents, "keep-less"),
                    ScopedEventsSorted = Table.Sort(ScopedThreadEvents, {"ts"}),
                    StartTimeAdded = Table.AddColumn(
                        ScopedEventsSorted, "Start Time (ms)", each List.Max({[ts] - ThreadStartTimestamp, 0}) / 1000
                    ),
                    RecalcStyleIterationAdded = AddRecalcStyleIterationColumn(StartTimeAdded),
                    TotalTimeAdded = Table.AddColumn(
                        RecalcStyleIterationAdded, "Total Time (ms)", each if [dur] <> null then [dur] / 1000 else null
                    ),
                    ActivityAdded = AddActivityColumn(TotalTimeAdded),
                    KeysAdded = AddKeyColumns(ActivityAdded)
                in
                    KeysAdded,
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
            ExpandSelectorStats = (sourceTable as table) as table =>
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
                            "Elapsed",
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
             * Returns a dictionary (as a record) whose field names are keys to the records in `records`.
             */
            ToDictionary = (records as list, key as text) as record =>
                List.Accumulate(
                    records,
                    [],
                    (accumRecord, current) =>
                        Record.AddField(accumRecord, Text.From(Record.Field(current, key)), current)
                ),
            /*
             * Returns a record as a dictionary, whose property names are keys to nested aggregate 
             * records. The aggregate record contains the selector stats in aggregate for the selector stat that 
             * occurred at that key.
             */
            BuildSelectorStatsAggregates = (sourceTable as table) as record =>
                let
                    SelectorStatsRows = Table.SelectRows(sourceTable, each [name] = "SelectorStats"),
                    SelectorStatsExpanded = ExpandSelectorStats(SelectorStatsRows),
                    SelectorStatsAggregatedByTimestamp = Table.Group(
                        SelectorStatsExpanded,
                        "Key",
                        {
                            {"Elapsed (us)", each List.Sum([Elapsed])},
                            {"Match Attempts", each List.Sum([Match Attempts])},
                            {"Match Count", each List.Sum([Match Count])},
                            {"Reject Count", each List.Sum([Reject Count])},
                            {"Fast Reject Count", each List.Sum([Fast Reject Count])},
                            {"Slow Reject Count", each List.Sum([Slow Reject Count])},
                            {"Selector Count", each List.Count([Selector])}
                        }
                    ),
                    SelectorStatsRecords = Table.ToRecords(SelectorStatsAggregatedByTimestamp),
                    SelectorStatsDictionary = ToDictionary(SelectorStatsRecords, "Key")
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
                                Record.Field(SelectorStatsDictionary, [Key])[#"Elapsed (us)"]
                            else
                                null
                    ),
                    MatchAttemptsAdded = Table.AddColumn(
                        ElapsedTotalAdded,
                        "Match Attempts",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Match Attempts]
                            else
                                null
                    ),
                    MatchCountAdded = Table.AddColumn(
                        MatchAttemptsAdded,
                        "Match Count",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Match Count]
                            else
                                null
                    ),
                    RejectCountAdded = Table.AddColumn(
                        MatchCountAdded,
                        "Reject Count",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Reject Count]
                            else
                                null
                    ),
                    FastRejectCountAdded = Table.AddColumn(
                        RejectCountAdded,
                        "Fast Reject Count",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Fast Reject Count]
                            else
                                null
                    ),
                    SlowRejectCountAdded = Table.AddColumn(
                        FastRejectCountAdded,
                        "Slow Reject Count",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Slow Reject Count]
                            else
                                null
                    ),
                    SelectorCountAdded = Table.AddColumn(
                        SlowRejectCountAdded,
                        "Selector Count",
                        each
                            if [name] = "SelectorStats" then
                                Record.Field(SelectorStatsDictionary, [Key])[Selector Count]
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
            UnrelatedEventsRemoved = RemoveUnrelatedEvents(BasicTraceEvents, "keep-more"),
            // Filter to just the events on the render threads
            RenderThreadIds = GetRenderThreadIds(UnrelatedEventsRemoved),
            RenderThreadEvents = GetRenderThreadEvents(UnrelatedEventsRemoved, RenderThreadIds),
            // Expand columns that have records
            ArgsExpanded = ExpandArgs(RenderThreadEvents),
            // Add post-expansion columns
            SelectorStatsAdded = AddSelectorStatsColumns(ArgsExpanded),
            // Remove, re-order and assign types to columns
            ColumnsRemoved = Table.RemoveColumns(SelectorStatsAdded, {"metadata", "selector_timings"}),
            ColumnsRenamed = Table.RenameColumns(ColumnsRemoved, {{"tid", "Thread ID"}}),
            ColumnsReordered = Table.ReorderColumns(
                ColumnsRenamed,
                {
                    "Thread ID",
                    "Start Time (ms)",
                    "Activity",
                    "Total Time (ms)",
                    "Recalc Style Iteration",
                    "Element Count",
                    "Elapsed (us)",
                    "Match Attempts",
                    "Match Count",
                    "Reject Count",
                    "Fast Reject Count",
                    "Slow Reject Count",
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
                    {"Total Time (ms)", type number},
                    {"Recalc Style Iteration", type number},
                    {"Element Count", type number},
                    {"Elapsed (us)", type number},
                    {"Match Attempts", type number},
                    {"Match Count", type number},
                    {"Reject Count", type number},
                    {"Fast Reject Count", type number},
                    {"Slow Reject Count", type number},
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
                    {"s", type text},
                    {"ts", Int64.Type},
                    {"tts", Int64.Type},
                    {"name", type text},
                    {"cat", type text},
                    {"Key", type text},
                    {"Iteration Key", type text}
                }
            ),
            ColumnsSorted = Table.Sort(TypesChanged, {"Thread ID", "ts"}),
            FinalColumns = ColumnsSorted
        in
            FinalColumns
in
    StyleEvents
