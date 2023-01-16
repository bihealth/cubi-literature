function Div(a)
  if a.classes[1] ~= "quarto-figure" then return end
  a.content = a.content:walk({
    Plain = function(el) 
      if el.content[1].t == "Str" and string.find(el.content[1].text, "^Figure") then
        el.content[1] = pandoc.Strong(el.content[1])
      end
      return el
    end,
  })

  return a
end
