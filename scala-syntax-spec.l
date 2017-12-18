////////////////////////////// SCALA SYNTAX SPEC //////////////////////////////

// NEWLINE, SEMI-COLON, COMMENT
                        nl :  “new line character”
                      semi :  ';' |  nl {nl}
                   comment :  '/*' “any sequence of characters; nested comments are allowed” '*/'
                             |  '//' “any sequence of characters up to end of line”

// NUMBERS
              nonZeroDigit :  '1' | … | '9'
                     digit :  '0' | nonZeroDigit
                  hexDigit :  '0' | … | '9' | 'A' | … | 'F' | 'a' | … | 'f'
                hexNumeral :  '0' ('x' | 'X') hexDigit {hexDigit}
            decimalNumeral :  '0' | nonZeroDigit {digit}
            integerLiteral :  (decimalNumeral | hexNumeral) ['L' | 'l']

                 floatType :  'F' | 'f' | 'D' | 'd'
              exponentPart :  ('E' | 'e') ['+' | '-'] digit {digit}
      floatingPointLiteral :  digit {digit} '.' digit {digit} [exponentPart] [floatType]
                             |                '.' digit {digit} [exponentPart] [floatType]
                             |  digit {digit}                    exponentPart  [floatType]
                             |  digit {digit}                   [exponentPart]  floatType

// TEXT
                     lower :  'a' | … | 'z' // and Unicode category Ll
                     upper :  'A' | … | 'Z' | '\$' | '_'  // and Unicode category Lu
                     delim :  '`' | ''' | '"' | '.' | ';' | ','
                     paren :  '(' | ')' | '[' | ']' | '{' | '}'
                    letter :  upper | lower // and Unicode categories Lo, Lt, Nl
                whiteSpace :  '\020' | '\009' | '\00D' | '\00A'
             printableChar :  '\020' | … | '\07F'
             charEscapeSeq :  '\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\')
             UnicodeEscape :  '\' 'u' {'u'} hexDigit hexDigit hexDigit hexDigit

charNoDoubleQuote          :  printableChar - '"'
charNoQuoteOrNewline       :  printableChar - ''' - nl
charNoDoubleQuoteOrNewline :  printableChar - '"' - nl

          characterLiteral :  ''' (charNoQuoteOrNewline | UnicodeEscape | charEscapeSeq) '''

             stringElement :  charNoDoubleQuoteOrNewline | UnicodeEscape | charEscapeSeq
            multiLineChars :  {['"'] ['"'] charNoDoubleQuote} {'"'}
             stringLiteral :  ('"' {stringElement} '"') | ('"""' multiLineChars '"""')
