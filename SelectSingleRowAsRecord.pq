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
            FinalState[SingleRecord]
in
    SelectSingleRowAsRecord
