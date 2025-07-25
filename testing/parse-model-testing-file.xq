xquery version "3.1";

  (:declare boundary-space preserve;:)
(:  LIBRARIES  :)
(:  NAMESPACES  :)
  (:declare default element namespace "http://www.wwp.northeastern.edu/ns/textbase";:)
  declare namespace array="http://www.w3.org/2005/xpath-functions/array";
  declare namespace map="http://www.w3.org/2005/xpath-functions/map";
  declare namespace output="http://www.w3.org/2010/xslt-xquery-serialization";
  declare namespace tei="http://www.tei-c.org/ns/1.0";
  declare namespace wwp="http://www.wwp.northeastern.edu/ns/textbase";
  declare namespace xhtml="http://www.w3.org/1999/xhtml";
(:  OPTIONS  :)
  declare option output:method "text";

(:~
  Grab the analogies test words from the Python model testing file in the Public Code Share, and create 
  character data that we can paste into our R test file.
  
  @author Ash Clark and Sarah Connell
  @since 2025
 :)

(:
    VARIABLES
 :)
  
  declare variable $model-testing-file-url := 'https://raw.githubusercontent.com/NEU-DSG/wwp-public-code-share/refs/heads/main/WordVectors/python/testing/analogies_test.txt';

(:
    FUNCTIONS
 :)
  

(:
    MAIN QUERY
 :)

if ( not(unparsed-text-available($model-testing-file-url)) ) then
  'File not available!'
else
  (:
    Target:
      (
      c("away", "off"),
      c("before", "after"),
      c("cause", "effects"),
      c("children", "parents"),
      c("come", "go"),
      c("day", "night"),
  :)
  let $lineSeq := tail(unparsed-text-lines($model-testing-file-url))
  let $rCodeLines :=
    for $line in $lineSeq
    let $cells := tokenize($line, '\t')
    let $word1 := $cells[1]
    let $relatedWords := tokenize($cells[2], '/')
    for $relatedWord in $relatedWords
    return
      'c("'||$word1||'", "'||$relatedWord||'"),'
  return
    string-join($rCodeLines, '
')

