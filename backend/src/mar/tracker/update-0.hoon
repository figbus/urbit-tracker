/+  *tracker
=*  as-octs  as-octs:mimes:html
::
|_  upd=update
++  grad  %noun
++  grow
  |%
  ++  noun  upd
  ++  json  (update:enjs upd)
  ++  mime  [/application/x-urb-tracker-update (as-octs (jam upd))]
  --
::
++  grab
  |%
  ++  noun  update
  ++  json  update:dejs
  ++  mime  |=([* =octs] ;;(update (cue q.octs)))
  --
--