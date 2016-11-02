#include <stdio.h>

void count(void);
void yyerror(char const *s);
void comment(void);
void error(const char *s);
int check_type(void);
int yyparse (void);
