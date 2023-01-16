local log = require "logging"

function Image(el)
  print("Boohoo")
  log.temp(el)
  print(pandoc.utils.stringify(el))
  log.temp(el.caption)
end
