/-  tracker
/+  default-agent, dbug
::
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0
  $:  [%0 =tasks:tracker =update-log:tracker]
  ==
+$  card  card:agent:gall
--
::
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this     .
    default  ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =.  state  [%0 tasks=[habits=~ dailies=~ todos=~] update-log=~]
  `this
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =/  prev  !<(versioned-state old-state)
  ?-  -.prev
      %0
    `this(state prev)
  ==
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  `this
      %tracker-update-0
    =/  upd  !<(update:tracker vase)
    =/  update-log-map  ((ordered-map time update:tracker) gth)
    =/  new-update-log  (put:update-log-map update-log now.bowl upd)
    ~&  >>  upd
    :-  ~[[%give %fact ~[/tasks] mark vase]]
    %=  this
        update-log
      new-update-log
    ::
        tasks
      ?-  -.upd
        %add-task         (add-task +.upd tasks)
        %update-task      (update-task +.upd tasks)
        %remove-task      (remove-task +.upd tasks)
        %complete-task    (complete-task now.bowl +.upd tasks)
        %uncomplete-task  (uncomplete-task +.upd tasks)
      ==
    ==
  ==
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:default path)
      [%tasks ~]
    :_  this
    :~  [%give %fact ~ %tracker-tasks !>(`tasks:tracker`tasks)]
    ==
  ==
++  on-leave  on-leave:default
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:default path)
      [%x %tasks ~]
    :^  ~  ~  %tracker-tasks
    !>  ^-  tasks:tracker
    tasks
  ==
++  on-agent  on-agent:default
++  on-arvo   on-arvo:default
++  on-fail   on-fail:default
--
|%
++  add-task
  |=  [upd=[=uuid:tracker =task:tracker] =tasks:tracker]
  ^-  tasks:tracker
  ?-  -.task.upd
    %habit
      %=  tasks
          habits
        =/  habits-inc  (~(run by habits.tasks) |=(=habit:tracker habit(order +(order.habit))))
        (~(put by habits-inc) uuid.upd habit.task.upd)
      ==
  ::
    %daily
      %=  tasks
          dailies
        =/  dailies-inc  (~(run by dailies.tasks) |=(=daily:tracker daily(order +(order.daily))))
        (~(put by dailies-inc) uuid.upd daily.task.upd)
      ==
  ::
    %todo
      %=  tasks
          todos
        =/  todos-inc  (~(run by todos.tasks) |=(=todo:tracker todo(order +(order.todo))))
        (~(put by todos-inc) uuid.upd todo.task.upd)
      ==
  ==
++  update-task
  |=  [upd=[=uuid:tracker update=task-update:tracker] =tasks:tracker]
  ^-  tasks:tracker
  ?-  -.update.upd
      %habit
    =/  update  habit.update.upd
    %=  tasks
        habits
      %+  ~(jab by habits.tasks)
        uuid.upd
      |=  =habit:tracker
      %=  habit
        title  (fall title.update title.habit)
        notes  (fall notes.update notes.habit)
      ==
    ==
  ::
      %daily
    =/  update  daily.update.upd
    %=  tasks
        dailies
      %+  ~(jab by dailies.tasks)
        uuid.upd
      |=  =daily:tracker
      %=  daily
        title           (fall title.update title.daily)
        notes           (fall notes.update notes.daily)
        start-date      (fall start-date.update start-date.daily)
        repeat-pattern  (fall repeat-pattern.update repeat-pattern.daily)
      ==
    ==
  ::
      %todo
    =/  update  todo.update.upd
    %=  tasks
        todos
      %+  ~(jab by todos.tasks)
        uuid.upd
      |=  =todo:tracker
      %=  todo
        title     (fall title.update title.todo)
        notes     (fall notes.update notes.todo)
        due-date  (fall due-date.update due-date.todo)
      ==
    ==
  ==
++  remove-task
  |=  [=uuid:tracker =tasks:tracker]
  ^-  tasks:tracker
  :+  =/  removed-habit  (~(get by habits.tasks) uuid)
      ?~  removed-habit
        habits.tasks
      =/  habits-new  (~(del by habits.tasks) uuid)
      %-  ~(run by habits-new)
      |=  =habit:tracker
      ?:  (lth order.habit order.u.removed-habit)
        habit
      habit(order (dec order.habit))
    ::
    =/  removed-daily  (~(get by dailies.tasks) uuid)
    ?~  removed-daily
      dailies.tasks
    =/  dailies-new  (~(del by dailies.tasks) uuid)
    %-  ~(run by dailies-new)
    |=  =daily:tracker
    ?:  (lth order.daily order.u.removed-daily)
      daily
    daily(order (dec order.daily))
  ::
  =/  removed-todo  (~(get by todos.tasks) uuid)
  ?~  removed-todo
    todos.tasks
  =/  todos-new  (~(del by todos.tasks) uuid)
  %-  ~(run by todos-new)
  |=  =todo:tracker
  ?:  (lth order.todo order.u.removed-todo)
    todo
  todo(order (dec order.todo))
++  complete-task
  |=  [now=@da =uuid:tracker =tasks:tracker]
  ^-  tasks:tracker
  :+  (map-task uuid habits.tasks |=(=habit:tracker habit(history [now history.habit])))
    (map-task uuid dailies.tasks |=(=daily:tracker daily(history [now history.daily])))
  (map-task uuid todos.tasks |=(=todo:tracker todo(completed-on `now)))
++  uncomplete-task
  |=  [=uuid:tracker =tasks:tracker]
  ^-  tasks:tracker
  :+  (map-task uuid habits.tasks |=(=habit:tracker habit(history ?~(history.habit ~ t.history.habit))))
    (map-task uuid dailies.tasks |=(=daily:tracker daily(history ?~(history.daily ~ t.history.daily))))
  (map-task uuid todos.tasks |=(=todo:tracker todo(completed-on ~)))
++  map-task 
  |*  [=uuid:tracker table=(map uuid:tracker *) =gate]
  ?.  (~(has by table) uuid)
   table 
  (~(jab by table) uuid gate)
--