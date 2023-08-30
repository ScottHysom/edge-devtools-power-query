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
                        FindValue = [FindValue]
                    ]
            ),
            LastRecord = List.Last(Result, null),
            FoundIndex =
                try if sortedList{LastRecord[MidIndex]} = findValue then LastRecord[MidIndex] else null
            otherwise
                null
        in
            FoundIndex
in
    BinarySearch