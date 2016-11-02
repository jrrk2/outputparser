#include "ansic.h"

void error(const char *s)
{
  fprintf(stderr, "%s\n", s);
}

int main(int argc, char **argv)
{
  yyparse();
}
