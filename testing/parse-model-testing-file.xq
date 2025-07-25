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
  (:declare option output:indent "no";:)

(:~
  
  
  @author Ash Clark
  @since 2024
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



