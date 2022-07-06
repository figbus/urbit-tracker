module Tests exposing (..)

import Derberos.Date.Delta
import Expect
import Tasks exposing (DaysOfWeek, MonthlyRepeatOn(..), RepeatPattern(..))
import Test exposing (Test)
import Time


suite : Test
suite =
    let
        time : Time.Posix
        time =
            Time.millisToPosix 0
    in
    Test.describe "isDue"
        [ Test.test "start after current day" <|
            \_ ->
                Tasks.isDue
                    Time.utc
                    time
                    { startDate = time |> Derberos.Date.Delta.addDays 1
                    , repeatPattern = DailyPattern { every = 1 }
                    }
                    |> Expect.equal False
        , Test.describe "daily"
            [ Test.test "every day" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 1)
                        { startDate = time
                        , repeatPattern = DailyPattern { every = 1 }
                        }
                        |> Expect.equal True
            , Test.test "every other day" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 2)
                        { startDate = time
                        , repeatPattern = DailyPattern { every = 2 }
                        }
                        |> Expect.equal True
            , Test.test "every other day (false)" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 1)
                        { startDate = time
                        , repeatPattern = DailyPattern { every = 2 }
                        }
                        |> Expect.equal False
            ]
        , let
            allWeek : DaysOfWeek
            allWeek =
                { sunday = True
                , monday = True
                , tuesday = True
                , wednesday = True
                , thursday = True
                , friday = True
                , saturday = True
                }
          in
          Test.describe "weekly"
            [ Test.describe "every week"
                [ Test.test "all week" <|
                    \_ ->
                        Tasks.isDue
                            Time.utc
                            (time |> Derberos.Date.Delta.addDays 7)
                            { startDate = time
                            , repeatPattern =
                                WeeklyPattern
                                    { every = 1
                                    , daysOfWeek = allWeek
                                    }
                            }
                            |> Expect.equal True
                , Test.test "false day" <|
                    \_ ->
                        Tasks.isDue
                            Time.utc
                            (time |> Derberos.Date.Delta.addDays 7)
                            { startDate = time
                            , repeatPattern =
                                WeeklyPattern
                                    { every = 1
                                    , daysOfWeek = { allWeek | thursday = False }
                                    }
                            }
                            |> Expect.equal False
                ]
            , Test.describe "every other week"
                [ Test.test "all week" <|
                    \_ ->
                        Tasks.isDue
                            Time.utc
                            (time |> Derberos.Date.Delta.addDays 14)
                            { startDate = time
                            , repeatPattern =
                                WeeklyPattern
                                    { every = 2
                                    , daysOfWeek = allWeek
                                    }
                            }
                            |> Expect.equal True
                , Test.test "all week (wrong week)" <|
                    \_ ->
                        Tasks.isDue
                            Time.utc
                            (time |> Derberos.Date.Delta.addDays 7)
                            { startDate = time
                            , repeatPattern =
                                WeeklyPattern
                                    { every = 2
                                    , daysOfWeek = allWeek
                                    }
                            }
                            |> Expect.equal False
                , Test.test "right week wrong day" <|
                    \_ ->
                        Tasks.isDue
                            Time.utc
                            (time |> Derberos.Date.Delta.addDays 14)
                            { startDate = time
                            , repeatPattern =
                                WeeklyPattern
                                    { every = 2
                                    , daysOfWeek = { allWeek | thursday = False }
                                    }
                            }
                            |> Expect.equal False
                ]
            ]
        , Test.describe "monthly"
            [ Test.test "next month (day of month)" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 31)
                        { startDate = time
                        , repeatPattern = MonthlyPattern { every = 1, repeatOn = DayOfMonth }
                        }
                        |> Expect.equal True
            , Test.test "next month (day of month) (false)" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 32)
                        { startDate = time
                        , repeatPattern = MonthlyPattern { every = 1, repeatOn = DayOfMonth }
                        }
                        |> Expect.equal False
            , Test.test "next month (day of week)" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 35)
                        { startDate = time
                        , repeatPattern = MonthlyPattern { every = 1, repeatOn = DayOfWeek }
                        }
                        |> Expect.equal True
            , Test.test "next month (day of week) (false)" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addDays 34)
                        { startDate = time
                        , repeatPattern = MonthlyPattern { every = 1, repeatOn = DayOfWeek }
                        }
                        |> Expect.equal False
            ]
        , Test.describe "yearly"
            [ Test.test "next year" <|
                \_ ->
                    Tasks.isDue
                        Time.utc
                        (time |> Derberos.Date.Delta.addYears 1)
                        { startDate = time
                        , repeatPattern = YearlyPattern { every = 1 }
                        }
                        |> Expect.equal True
            ]
        ]
