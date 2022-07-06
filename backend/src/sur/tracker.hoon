|%
::
+$  tasks
  $:  habits=(map uuid habit)
      dailies=(map uuid daily)
      todos=(map uuid todo)
  ==
::
+$  task-type
  $%  %habit
      %daily
      %todo
  ==
::
+$  task
  $%  [%habit =habit]
      [%daily =daily]
      [%todo =todo]
  ==
+$  habit
  $:  order=@u
      title=cord
      notes=cord
      history=(list time)
  ==
+$  daily
  $:  order=@u
      title=cord
      notes=cord
      start-date=time
      =repeat-pattern
      history=(list time)
  ==
+$  todo
  $:  order=@u
      title=cord
      notes=cord
      due-date=(unit time)
      completed-on=(unit time)
  ==
::
+$  task-update
  $%  [%habit habit=habit-update]
      [%daily daily=daily-update]
      [%todo todo=todo-update]
  ==
+$  habit-update  [title=(unit cord) notes=(unit cord)]
+$  daily-update
  $:  title=(unit cord)
      notes=(unit cord)
      start-date=(unit time)
      repeat-pattern=(unit repeat-pattern)
  ==
+$  todo-update  [title=(unit cord) notes=(unit cord) due-date=(unit (unit time))]
::
+$  repeat-pattern
  $%  [%daily every=@u]
      [%weekly every=@u =days-of-week]
      [%monthly every=@u repeat-on=monthly-repeat-on]
      [%yearly every=@u]
  ==
+$  days-of-week
  $:  sunday=bean
      monday=bean
      tuesday=bean
      wednesday=bean
      thursday=bean
      friday=bean
      saturday=bean
  ==
+$  monthly-repeat-on
  $%  %day-of-month
      %day-of-week
  ==
::
+$  uuid  cord
::
+$  update-log  ((mop time update) gth)
+$  update
  $%  [%add-task =uuid =task]
      [%update-task =uuid update=task-update]
      [%remove-task =uuid]
      [%complete-task =uuid]
      [%uncomplete-task =uuid]
  ==
--