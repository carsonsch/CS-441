Here are several code samples: 

code001.txt: Correct, a simple comparison. Note that the end; is not syntactically required, but is allowed. 

code002.txt: Also correct, a while loop. 

code003.txt: Also correct, nested parentheses. 

code004txt: gosub call, multiple statements on 1 line. 

code005.txt: gosub replaced with goto; this is a logic error, but it should compile. 

code006.txt: static semantic errors (multiple labels with same value) but this should compile. 

code007.txt: Syntax error on line 3 (multiple operators)

code008.txt: Syntax error on line 2 (missing semicolon)

code009.txt: Syntax error on line 3 (mismatched parentheses) 

code010.txt: Syntax error on line 6 / 13: goto keyword used as statement label. 

code011.txt: Static semantic error (gosub to nonexistent label), but this should compile. 
