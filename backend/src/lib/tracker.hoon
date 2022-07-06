/-  sur=tracker
=<  [sur .]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ++  tasks
    |=  =^tasks
    ^-  json
    %-  pairs
    :~  [%habits (habits habits.tasks)]
        [%dailies (dailies dailies.tasks)]
        [%todos (todos todos.tasks)]
    ==
  ++  habits
    |=  a=(map cord ^habit)
    ^-  json
    :-  %o
    %-  ~(rut by a)
    |=  [k=cord v=^habit]
    (habit v)
  ++  dailies
    |=  a=(map cord ^daily)
    ^-  json
    :-  %o
    %-  ~(rut by a)
    |=  [k=cord v=^daily]
    (daily v)
  ++  todos
    |=  a=(map cord ^todo)
    ^-  json
    :-  %o
    %-  ~(rut by a)
    |=  [k=cord v=^todo]
    (todo v)
  ++  habit
    |=  =^habit
    ^-  json
    %-  pairs
    :~  [%order (numb order.habit)]
        [%title s+title.habit]
        [%notes s+notes.habit]
        [%history a+(turn history.habit time)]
    ==
  ++  daily
    |=  =^daily
    ^-  json
    %-  pairs
    :~  [%order (numb order.daily)]
        [%title s+title.daily]
        [%notes s+notes.daily]
        [%start-date (time start-date.daily)]
        [%repeat-pattern (pairs ~[(repeat-pattern repeat-pattern.daily)])]
        [%history a+(turn history.daily time)]
    ==
  ++  todo
    |=  =^todo
    ^-  json
    %-  pairs
    :~  [%order (numb order.todo)]
        [%title s+title.todo]
        [%notes s+notes.todo]
        [%due-date ?~(due-date.todo ~ (time u.due-date.todo))]
        [%completed-on ?~(completed-on.todo ~ (time u.completed-on.todo))]
    ==
  ++  repeat-pattern
    |=  repeat=^repeat-pattern
    ^-  [cord json]
    ?-  -.repeat
        %daily
      :-  %daily
      %-  pairs
      :~  [%every (numb every.repeat)]
      ==
    ::
        %weekly
      :-  %weekly
      %-  pairs
      :~  [%every (numb every.repeat)]
          :-  %days-of-week
          =*  days  days-of-week.repeat
          %-  pairs
          :~  [%sunday b+sunday.days]
              [%monday b+monday.days]
              [%tuesday b+tuesday.days]
              [%wednesday b+wednesday.days]
              [%thursday b+thursday.days]
              [%friday b+friday.days]
              [%saturday b+saturday.days]
          ==
      ==
    ::
        %monthly
      :-  %monthly
      %-  pairs
      :~  [%every (numb every.repeat)]
          [%repeat-on s+repeat-on.repeat]
      ==
    ::
        %yearly
      :-  %yearly
      %-  pairs
      :~  [%every (numb every.repeat)]
      ==
    ==
  ++  update
    |=  upd=^update
    ^-  json
    |^  (pairs ~[(encode upd)])
    ::
    ++  encode
      |=  upd=^update
      ^-  [cord json]
      ?-  -.upd
          %add-task
        :-  %add-task
        %-  pairs
        :~  [%uuid s+uuid.upd]
            [%task (pairs ~[(task task.upd)])]
        ==
      ::
          %update-task
        :-  %update-task 
        %-  pairs
        :~  [%uuid s+uuid.upd]
            [%update (pairs ~[(task-update update.upd)])]
        ==
      ::
          %remove-task
        :-  %remove-task 
        s+uuid.upd
      ::
          %complete-task
        :-  %complete-task 
        s+uuid.upd
      ::
          %uncomplete-task
        :-  %uncomplete-task 
        s+uuid.upd
      ==
    ++  task
      |=  =^task
      ^-  [cord json]
      ?-  -.task
        %habit  [%habit (habit habit.task)]
        %daily  [%daily (daily daily.task)]
        %todo   [%todo (todo todo.task)]
      ==
    ++  task-update
      |=  update=^task-update
      ^-  [cord json]
      ?-  -.update
          %habit
        :-  %habit
        =*  habit  habit.update
        %-  unit-pairs
        :~  ?~(title.habit ~ `[%title s+u.title.habit])
            ?~(notes.habit ~ `[%notes s+u.notes.habit])
        ==
      ::
         %daily
        :-  %daily
        =*  daily  daily.update
        %-  unit-pairs
        :~  ?~(title.daily ~ `[%title s+u.title.daily])
            ?~(notes.daily ~ `[%notes s+u.notes.daily])
            ?~(start-date.daily ~ `[%start-date (time u.start-date.daily)])
            ?~(repeat-pattern.daily ~ `[%repeat-pattern (pairs ~[(repeat-pattern u.repeat-pattern.daily)])])
        ==
      ::
          %todo
        :-  %todo
        =*  todo  todo.update
        %-  unit-pairs
        :~  ?~(title.todo ~ `[%title s+u.title.todo])
            ?~(notes.todo ~ `[%notes s+u.notes.todo])
            ?~(due-date.todo ~ `[%due-date ?~(u.due-date.todo ~ (time u.u.due-date.todo))])
        ==
      ==
    --
  --
++  dejs
  =,  dejs:format
  |%
  ++  update
    |=  jon=json
    ^-  ^update
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%add-task add-task]
          [%update-task update-task]
          [%remove-task so]
          [%complete-task so]
          [%uncomplete-task so]
      ==
    ++  add-task
      %-  ot
      :~  [%uuid so]
          [%task task]
      ==
    ++  update-task
      %-  ot
      :~  [%uuid so]
          [%update task-update]
      ==
    ++  task
      %-  of
      :~  [%habit habit]
          [%daily daily]
          [%todo todo]
      ==
    ++  habit
      %-  ot
      :~  [%order ni]
          [%title so]
          [%notes so]
          [%history (ar di)]
      ==
    ++  daily
      %-  ot
      :~  [%order ni]
          [%title so]
          [%notes so]
          [%start-date di]
          [%repeat-pattern repeat-pattern]
          [%history (ar di)]
      ==
    ++  repeat-pattern
      %-  of
      :~  [%daily (ot ~[[%every ni]])]
          [%weekly weekly]
          [%monthly monthly]
          [%yearly (ot ~[[%every ni]])]
      ==
    ++  weekly
      %-  ot
      :~  [%every ni]
          :-  %days-of-week
          %-  ot
          :~  [%sunday bo]
              [%monday bo]
              [%tuesday bo]
              [%wednesday bo]
              [%thursday bo]
              [%friday bo]
              [%saturday bo]
          ==
      ==
    ++  monthly
      %-  ot
      :~  [%every ni]
          [%repeat-on (su (perk ~[%day-of-month %day-of-week]))]
      ==
    ++  todo
      %-  ot
      :~  [%order ni]
          [%title so]
          [%notes so]
          [%due-date (mu di)]
          [%completed-on (mu di)]
      ==
    ++  task-update
      %-  of
      :~  [%habit habit-update]
          [%daily daily-update]
          [%todo todo-update]
      ==
    ++  habit-update
      %-  ou
      :~  [%title (uf ~ (cu some so))]
          [%notes (uf ~ (cu some so))]
      ==
    ++  daily-update
      %-  ou
      :~  [%title (uf ~ (cu some so))]
          [%notes (uf ~ (cu some so))]
          [%start-date (uf ~ (cu some di))]
          [%repeat-pattern (uf ~ (cu some repeat-pattern))]
      ==
    ++  todo-update
      %-  ou
      :~  [%title (uf ~ (mu so))]
          [%notes (uf ~ (mu so))]
          [%due-date (uf ~ (cu some (mu di)))]
      ==
    --
  --
++  unit-pairs
  |=  pairs=(list (unit [@t json]))
  ^-  json
  %-  pairs:enjs:format
  %+  murn
    pairs
  |=(pair=(unit [@t json]) pair)
--