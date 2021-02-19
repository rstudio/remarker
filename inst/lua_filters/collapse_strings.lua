local List = require 'pandoc.List'

function Inlines (inlines)
  inlines = unwrap_quotes(inlines)
  inlines = collapse_strings(inlines)
  return inlines
end


-- Returns a pandoc.Str object representing a single or double quote.
function quote_str (quote_type)
  if (quote_type == "DoubleQuote") then
    return(pandoc.Str '"')
  elseif (quote_type == "SingleQuote") then
    return(pandoc.Str "'")
  else
    error("Unknown quote type: " .. quote_type)
  end
end


-- Traverses an Inlines List, returns a new List with Quoted objects replaced
-- by a Str object for the opening quote, the Quoted object's inlines, and then
-- a Str object for the closing quote.
function unwrap_quotes (inlines)
  out_inlines = List:new{}

  for i = 1, #inlines, 1 do
    if (inlines[i].t == "Quoted") then
      quote_type    = inlines[i].c[1]
      quote_inlines = inlines[i].c[2]

      out_inlines:insert(quote_str(quote_type))
      out_inlines:extend(quote_inlines)
      out_inlines:insert(quote_str(quote_type))

    else
      table.insert(out_inlines, inlines[i])
    end

  end

  return out_inlines
end


-- Traverses an Inlines List, collapsing Str and Spaces into a single Str.
function collapse_strings (inlines)
  -- Go from end to start to avoid problems with shifting indices.
  for i = #inlines-1, 1, -1 do
    if (inlines[i].t == "Str" and inlines[i+1].t == "Space") then
      inlines[i].c = inlines[i].c .. " "
      inlines:remove(i+1)

    elseif (inlines[i].t == "Space" and inlines[i+1].t == "Str") then
      inlines[i].t = "Str"
      inlines[i].c = " " .. inlines[i+1].c
      inlines:remove(i+1)

    elseif (inlines[i].t == "Str" and inlines[i+1].t == "Str") then
      inlines[i].c = inlines[i].c .. inlines[i+1].c
      inlines:remove(i+1)

    elseif (inlines[i].t == "Space" and inlines[i+1].t == "Space") then
      inlines[i].t = "Str"
      inlines[i].c = "  "
      inlines:remove(i+1)
    end
  end

  return inlines
end
