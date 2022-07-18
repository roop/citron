// This file is part of the Citron Parser Generator,
// derived from the Lemon Parser Generator.
//
// Authors disclaimed copyright
// Public domain code

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#define ISSPACE(X) isspace((unsigned char)(X))
#define ISDIGIT(X) isdigit((unsigned char)(X))
#define ISALNUM(X) isalnum((unsigned char)(X))
#define ISALPHA(X) isalpha((unsigned char)(X))
#define ISUPPER(X) isupper((unsigned char)(X))
#define ISLOWER(X) islower((unsigned char)(X))
#define TOUPPER(X) toupper((unsigned char)(X))


#ifndef __WIN32__
#   if defined(_WIN32) || defined(WIN32)
#       define __WIN32__
#   endif
#endif

#ifdef __WIN32__
#ifdef __cplusplus
extern "C" {
#endif
extern int access(const char *path, int mode);
#ifdef __cplusplus
}
#endif
#else
#include <unistd.h>
#endif

/* #define PRIVATE static */
#define PRIVATE

#ifdef TEST
#define MAXRHS 5       /* Set low to exercise exception code */
#define MAX_ERR_CAPTURE_TOKENS 5
#define MAX_ERR_CAPTURE_TOKEN_SEQUENCES 5
#else
#define MAXRHS 1000
#define MAX_ERR_CAPTURE_TOKENS 1000
#define MAX_ERR_CAPTURE_TOKEN_SEQUENCES 1000
#endif

static int showPrecedenceConflict = 0;
static char *msort(char*,char**,int(*)(const char*,const char*));

/*
** Compilers are getting increasingly pedantic about type conversions
** as C evolves ever closer to Ada....  To work around the latest problems
** we have to define the following variant of strlen().
*/
#define lemonStrlen(X)   ((int)strlen(X))

/*
** Compilers are starting to complain about the use of sprintf() and strcpy(),
** saying they are unsafe.  So we define our own versions of those routines too.
**
** There are three routines here:  lemon_sprintf(), lemon_vsprintf(), and
** lemon_addtext(). The first two are replacements for sprintf() and vsprintf().
** The third is a helper routine for vsnprintf() that adds texts to the end of a
** buffer, making sure the buffer is always zero-terminated.
**
** The string formatter is a minimal subset of stdlib sprintf() supporting only
** a few simply conversions:
**
**   %d
**   %s
**   %.*s
**
*/
static void lemon_addtext(
  char *zBuf,           /* The buffer to which text is added */
  int *pnUsed,          /* Slots of the buffer used so far */
  const char *zIn,      /* Text to add */
  int nIn,              /* Bytes of text to add.  -1 to use strlen() */
  int iWidth            /* Field width.  Negative to left justify */
){
  if( nIn<0 ) for(nIn=0; zIn[nIn]; nIn++){}
  while( iWidth>nIn ){ zBuf[(*pnUsed)++] = ' '; iWidth--; }
  if( nIn==0 ) return;
  memcpy(&zBuf[*pnUsed], zIn, nIn);
  *pnUsed += nIn;
  while( (-iWidth)>nIn ){ zBuf[(*pnUsed)++] = ' '; iWidth++; }
  zBuf[*pnUsed] = 0;
}
static int lemon_vsprintf(char *str, const char *zFormat, va_list ap){
  int i, j, k, c;
  int nUsed = 0;
  const char *z;
  char zTemp[50];
  str[0] = 0;
  for(i=j=0; (c = zFormat[i])!=0; i++){
    if( c=='%' ){
      int iWidth = 0;
      lemon_addtext(str, &nUsed, &zFormat[j], i-j, 0);
      c = zFormat[++i];
      if( ISDIGIT(c) || (c=='-' && ISDIGIT(zFormat[i+1])) ){
        if( c=='-' ) i++;
        while( ISDIGIT(zFormat[i]) ) iWidth = iWidth*10 + zFormat[i++] - '0';
        if( c=='-' ) iWidth = -iWidth;
        c = zFormat[i];
      }
      if( c=='d' ){
        int v = va_arg(ap, int);
        if( v<0 ){
          lemon_addtext(str, &nUsed, "-", 1, iWidth);
          v = -v;
        }else if( v==0 ){
          lemon_addtext(str, &nUsed, "0", 1, iWidth);
        }
        k = 0;
        while( v>0 ){
          k++;
          zTemp[sizeof(zTemp)-k] = (v%10) + '0';
          v /= 10;
        }
        lemon_addtext(str, &nUsed, &zTemp[sizeof(zTemp)-k], k, iWidth);
      }else if( c=='s' ){
        z = va_arg(ap, const char*);
        lemon_addtext(str, &nUsed, z, -1, iWidth);
      }else if( c=='.' && memcmp(&zFormat[i], ".*s", 3)==0 ){
        i += 2;
        k = va_arg(ap, int);
        z = va_arg(ap, const char*);
        lemon_addtext(str, &nUsed, z, k, iWidth);
      }else if( c=='%' ){
        lemon_addtext(str, &nUsed, "%", 1, 0);
      }else{
        fprintf(stderr, "illegal format\n");
        exit(1);
      }
      j = i+1;
    }
  }
  lemon_addtext(str, &nUsed, &zFormat[j], i-j, 0);
  return nUsed;
}
static int lemon_sprintf(char *str, const char *format, ...){
  va_list ap;
  int rc;
  va_start(ap, format);
  rc = lemon_vsprintf(str, format, ap);
  va_end(ap);
  return rc;
}
static void lemon_strcpy(char *dest, const char *src){
  while( (*(dest++) = *(src++))!=0 ){}
}
static void lemon_strcat(char *dest, const char *src){
  while( *dest ) dest++;
  lemon_strcpy(dest, src);
}


/* a few forward declarations... */
struct rule;
struct lemon;
struct action;
struct token_sequence;

static struct action *Action_new(void);
static struct action *Action_sort(struct action *);

/********** From the file "build.h" ************************************/
void FindRulePrecedences(struct lemon*);
void FindFirstSets(struct lemon*);
void FindStates(struct lemon*);
void FindLinks(struct lemon*);
void FindFollowSets(struct lemon*);
void FindActions(struct lemon*);

/********* From the file "configlist.h" *********************************/
void Configlist_init(void);
struct config *Configlist_add(struct rule *, int);
struct config *Configlist_addbasis(struct rule *, int);
void Configlist_closure(struct lemon *);
void Configlist_sort(void);
void Configlist_sortbasis(void);
struct config *Configlist_return(void);
struct config *Configlist_basis(void);
void Configlist_eat(struct config *);
void Configlist_reset(void);

/********* From the file "error.h" ***************************************/
void ErrorMsg(const char *, int,const char *, ...);

/****** From the file "option.h" ******************************************/
enum option_type { OPT_FLAG=1,  OPT_INT,  OPT_DBL,  OPT_STR,
         OPT_FFLAG, OPT_FINT, OPT_FDBL, OPT_FSTR};
struct s_options {
  enum option_type type;
  const char *label;
  char *arg;
  const char *message;
};
int    OptInit(char**,int,struct s_options*,char **,FILE*);
void   OptPrint(void);

/******** From the file "parse.h" *****************************************/
void Parse(struct lemon *lemp);

void CheckTypeDefinitions(struct lemon *lemp);
void CheckCodeBlocks(struct lemon *lemp);
void CheckErrorCaptureDirectives(struct lemon *lemp);

/********* From the file "plink.h" ***************************************/
struct plink *Plink_new(void);
void Plink_add(struct plink **, struct config *);
void Plink_copy(struct plink **, struct plink *);
void Plink_delete(struct plink *);

/********** From the file "report.h" *************************************/
void Reprint(struct lemon *);
void ReportOutput(struct lemon *);
void ReportTable(struct lemon *);
void CompressTables(struct lemon *);
void ResortStates(struct lemon *);

/********** From the file "set.h" ****************************************/
void  SetSize(int);             /* All sets will be of size N */
char *SetNew(void);               /* A new set for element 0..N */
void  SetFree(char*);             /* Deallocate a set */
int SetAdd(char*,int);            /* Add element to a set */
int SetUnion(char *,char *);    /* A <- A U B, thru element N */
#define SetFind(X,Y) (X[Y])       /* True if Y is in set X */

/********** From the file "struct.h" *************************************/
/*
** Principal data structures for the LEMON parser generator.
*/

typedef enum {LEMON_FALSE=0, LEMON_TRUE} Boolean;

/* Symbols (terminals and nonterminals) of the grammar are stored
** in the following: */
enum symbol_type {
  TERMINAL,
  NONTERMINAL,
  MULTITERMINAL
};
enum e_assoc {
    LEFT,
    RIGHT,
    NONE,
    UNK
};
struct symbol {
  const char *name;        /* Name of the symbol */
  int index;               /* Index number for this symbol */
  enum symbol_type type;   /* Symbols are all either TERMINALS or NTs */
  struct rule *rule;       /* Linked list of rules of this (if an NT) */
  struct symbol *fallback; /* fallback token in case this token doesn't parse */
  int prec;                /* Precedence if defined (-1 otherwise) */
  enum e_assoc assoc;      /* Associativity if precedence is defined */
  char *firstset;          /* First-set for all rules of this symbol */
  Boolean lambda;          /* True if NT and can generate an empty string */
  int useCnt;              /* Number of times used */
  char *destructor;        /* Code which executes whenever this symbol is
                           ** popped from the stack during error processing */
  int destLineno;          /* Line number for start of destructor.  Set to
                           ** -1 for duplicate destructors. */
  char *datatype;          /* The data type of information held by this
                           ** object. Only used if type==NONTERMINAL */
  int dtnum;               /* The data type number.  In the parser, the value
                           ** stack is a union.  The .yy%d element of this
                           ** union is the correct data type for this object */
  const char *code;        /* Default code block. Only used if type==NONTERMINAL */
  int codeLineNumber;      /* Line number at which code begins. Only used if type==NONTERMINAL */

  /* The following fields are used by MULTITERMINALs only */
  int nsubsym;             /* Number of constituent symbols in the MULTI */
  struct symbol **subsym;  /* Array of constituent symbols */
  /* The following fields are used for error capturing */
  int error_capture_line;  /* Line number of error capture directive */
  struct token_sequence *error_capture_end_before_sequences;
  int num_error_capture_end_before_sequences;
  struct token_sequence *error_capture_end_after_sequences;
  int num_error_capture_end_after_sequences;
};

/* A token sequence that triggers error capturing */
union token_sequence_tokens {
  struct symbol *single_token; // In most cases, we'll have just one token in the sequence (token_sequence.count == 1)
  struct symbol **multiple_tokens; // In some cases, we can have multiple tokens in the sequence (count > 1)
};
struct token_sequence {
  int count;
  union token_sequence_tokens tokens;
};

/* Each production rule in the grammar is stored in the following
** structure.  */
struct rule {
  struct symbol *lhs;      /* Left-hand side of the rule */
  const char *lhsalias;    /* Alias for the LHS (NULL if none) */
  int lhsStart;            /* True if left-hand side is the start symbol */
  int ruleline;            /* Line number for the rule */
  int nrhs;                /* Number of RHS symbols */
  struct symbol **rhs;     /* The RHS symbols */
  const char **rhsalias;   /* An alias for each RHS symbol (NULL if none) */
  int line;                /* Line number at which code begins */
  const char *code;        /* The code executed when this rule is reduced */
  int noCode;              /* True if this rule has no associated C code */
  struct symbol *precsym;  /* Precedence symbol for this rule */
  int index;               /* An index number for this rule */
  int iRule;               /* Rule number as used in the generated tables */
  Boolean canReduce;       /* True if this rule is ever reduced */
  Boolean doesReduce;      /* Reduce actions occur after optimization */
  struct rule *nextlhs;    /* Next rule with the same LHS */
  struct rule *next;       /* Next rule in the global list */
};

/* A configuration is a production rule of the grammar together with
** a mark (dot) showing how much of that rule has been processed so far.
** Configurations also contain a follow-set which is a list of terminal
** symbols which are allowed to immediately follow the end of the rule.
** Every configuration is recorded as an instance of the following: */
enum cfgstatus {
  COMPLETE,
  INCOMPLETE
};
struct config {
  struct rule *rp;         /* The rule upon which the configuration is based */
  int dot;                 /* The parse point */
  char *fws;               /* Follow-set for this configuration only */
  struct plink *fplp;      /* Follow-set forward propagation links */
  struct plink *bplp;      /* Follow-set backwards propagation links */
  struct state *stp;       /* Pointer to state which contains this */
  enum cfgstatus status;   /* used during followset and shift computations */
  struct config *next;     /* Next configuration in the state */
  struct config *bp;       /* The next basis configuration */
};

enum e_action {
  SHIFT,
  ACCEPT,
  REDUCE,
  ERROR,
  SSCONFLICT,              /* A shift/shift conflict */
  SRCONFLICT,              /* Was a reduce, but part of a conflict */
  RRCONFLICT,              /* Was a reduce, but part of a conflict */
  SH_RESOLVED,             /* Was a shift.  Precedence resolved conflict */
  RD_RESOLVED,             /* Was reduce.  Precedence resolved conflict */
  NOT_USED,                /* Deleted by compression */
  SHIFTREDUCE              /* Shift first, then reduce */
};

/* Every shift or reduce operation is stored as one of the following */
struct action {
  struct symbol *sp;       /* The look-ahead symbol */
  enum e_action type;
  union {
    struct state *stp;     /* The new state, if a shift */
    struct rule *rp;       /* The rule, if a reduce */
  } x;
  struct symbol *spOpt;    /* SHIFTREDUCE optimization to this symbol */
  struct action *next;     /* Next action for this state */
  struct action *collide;  /* Next action with the same hash */
};

/* Each state of the generated parser's finite state machine
** is encoded as an instance of the following structure. */
struct state {
  struct config *bp;       /* The basis configurations for this state */
  struct config *cfp;      /* All configurations in this set */
  int statenum;            /* Sequential number for this state */
  struct action *ap;       /* List of actions for this state */
  int nTknAct, nNtAct;     /* Number of actions on terminals and nonterminals */
  int iTknOfst, iNtOfst;   /* yy_action[] offset for terminals and nonterms */
  int iDfltReduce;         /* Default action is to REDUCE by this rule */
  struct rule *pDfltReduce;/* The default REDUCE rule. */
  int autoReduce;          /* True if this is an auto-reduce state */
};
#define NO_OFFSET (-2147483647)

/* A followset propagation link indicates that the contents of one
** configuration followset should be propagated to another whenever
** the first changes. */
struct plink {
  struct config *cfp;      /* The configuration to which linked */
  struct plink *next;      /* The next propagate link */
};

/* The state vector for the entire parser generator is recorded as
** follows.  (LEMON uses no global variables and makes little use of
** static variables.  Fields in the following structure can be thought
** of as begin global variables in the program.) */
struct lemon {
  struct state **sorted;   /* Table of states sorted by state number */
  struct rule *rule;       /* List of all rules */
  struct rule *startRule;  /* First rule */
  int nstate;              /* Number of states */
  int nxstate;             /* nstate with tail degenerate states removed */
  int nrule;               /* Number of rules */
  int nsymbol;             /* Number of terminal and nonterminal symbols */
  int nterminal;           /* Number of terminal symbols */
  struct symbol **symbols; /* Sorted array of pointers to symbols */
  int errorcnt;            /* Number of errors */
  struct symbol *wildcard; /* Token that matches anything */
  char *className;         /* Name of the generated parser class */
  char *extraClassMembers; /* Extra members to be part of the parser class */
  char *tokentype;         /* Type of terminal symbols in the parser stack */
  char *vartype;           /* The default type of non-terminal symbols */
  const char *defaultCodeBlock;  /* Code to use as the default code block for default-typed non-terminals */
  int defaultCodeBlockLineNumber;  /* Line number of the start of default code block */
  char *start;             /* Name of the start symbol for the grammar */
  char *preface;           /* Code to put at the start of the generated file */
  int prefaceLineNumber; /* Line number of preface */
  char *epilogue;          /* Code to put at the end of the generated file */
  int epilogueLineNumber; /* Line number of epilogue */
  char *filename;          /* Name of the input file */
  char *outname;           /* Name of the current output file */
  char *tokenprefix;       /* A prefix added to token names in the generated enum */
  char *nonterminalprefix; /* A prefix added to nonterminal names in the generated enum */
  int nconflict;           /* Number of parsing conflicts */
  int nactiontab;          /* Number of entries in the yy_action[] table */
  int tablesize;           /* Total table size of all tables in bytes */
  int basisflag;           /* Print only basis configurations */
  int has_fallback;        /* True if any %fallback is seen in the grammar */
  int has_error_capture;   /* True if any %capture_errors is seen in the grammar */
  char *argv0;             /* Name of the program */
};

#define MemoryCheck(X) if((X)==0){ \
  extern void memory_error(); \
  memory_error(); \
}

/**************** From the file "table.h" *********************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/
/* Routines for handling a strings */

const char *Strsafe(const char *);

void Strsafe_init(void);
int Strsafe_insert(const char *);
const char *Strsafe_find(const char *);

/* Routines for handling symbols of the grammar */

struct symbol *Symbol_new(const char *);
int Symbolcmpp(const void *, const void *);
void Symbol_init(void);
int Symbol_insert(struct symbol *, const char *);
struct symbol *Symbol_find(const char *);
struct symbol *Symbol_Nth(int);
int Symbol_count(void);
struct symbol **Symbol_arrayof(void);

/* Routines to manage the state table */

int Configcmp(const char *, const char *);
struct state *State_new(void);
void State_init(void);
int State_insert(struct state *, struct config *);
struct state *State_find(struct config *);
struct state **State_arrayof(void);

/* Routines used for efficiency in Configlist_add */

void Configtable_init(void);
int Configtable_insert(struct config *);
struct config *Configtable_find(struct config *);
void Configtable_clear(int(*)(struct config *));

/****************** From the file "action.c" *******************************/
/*
** Routines processing parser actions in the LEMON parser generator.
*/

/* Allocate a new parser action */
static struct action *Action_new(void){
  static struct action *freelist = 0;
  struct action *newaction;

  if( freelist==0 ){
    int i;
    int amt = 100;
    freelist = (struct action *)calloc(amt, sizeof(struct action));
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new parser action.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  newaction = freelist;
  freelist = freelist->next;
  return newaction;
}

/* Compare two actions for sorting purposes.  Return negative, zero, or
** positive if the first action is less than, equal to, or greater than
** the first
*/
static int actioncmp(
  struct action *ap1,
  struct action *ap2
){
  int rc;
  rc = ap1->sp->index - ap2->sp->index;
  if( rc==0 ){
    rc = (int)ap1->type - (int)ap2->type;
  }
  if( rc==0 && (ap1->type==REDUCE || ap1->type==SHIFTREDUCE) ){
    rc = ap1->x.rp->index - ap2->x.rp->index;
  }
  if( rc==0 ){
    rc = (int) (ap2 - ap1);
  }
  return rc;
}

/* Sort parser actions */
static struct action *Action_sort(
  struct action *ap
){
  ap = (struct action *)msort((char *)ap,(char **)&ap->next,
                              (int(*)(const char*,const char*))actioncmp);
  return ap;
}

void Action_add(
  struct action **app,
  enum e_action type,
  struct symbol *sp,
  char *arg
){
  struct action *newaction;
  newaction = Action_new();
  newaction->next = *app;
  *app = newaction;
  newaction->type = type;
  newaction->sp = sp;
  newaction->spOpt = 0;
  if( type==SHIFT ){
    newaction->x.stp = (struct state *)arg;
  }else{
    newaction->x.rp = (struct rule *)arg;
  }
}
/********************** New code to implement the "acttab" module ***********/
/*
** This module implements routines use to construct the yy_action[] table.
*/

/*
** The state of the yy_action table under construction is an instance of
** the following structure.
**
** The yy_action table maps the pair (state_number, lookahead) into an
** action_number.  The table is an array of integers pairs.  The state_number
** determines an initial offset into the yy_action array.  The lookahead
** value is then added to this initial offset to get an index X into the
** yy_action array. If the aAction[X].lookahead equals the value of the
** of the lookahead input, then the value of the action_number output is
** aAction[X].action.  If the lookaheads do not match then the
** default action for the state_number is returned.
**
** All actions associated with a single state_number are first entered
** into aLookahead[] using multiple calls to acttab_action().  Then the
** actions for that single state_number are placed into the aAction[]
** array with a single call to acttab_insert().  The acttab_insert() call
** also resets the aLookahead[] array in preparation for the next
** state number.
*/
struct lookahead_action {
  int lookahead;             /* Value of the lookahead token */
  int action;                /* Action to take on the given lookahead */
};
typedef struct acttab acttab;
struct acttab {
  int nAction;                 /* Number of used slots in aAction[] */
  int nActionAlloc;            /* Slots allocated for aAction[] */
  struct lookahead_action
    *aAction,                  /* The yy_action[] table under construction */
    *aLookahead;               /* A single new transaction set */
  int mnLookahead;             /* Minimum aLookahead[].lookahead */
  int mnAction;                /* Action associated with mnLookahead */
  int mxLookahead;             /* Maximum aLookahead[].lookahead */
  int nLookahead;              /* Used slots in aLookahead[] */
  int nLookaheadAlloc;         /* Slots allocated in aLookahead[] */
};

/* Return the number of entries in the yy_action table */
#define acttab_size(X) ((X)->nAction)

/* The value for the N-th entry in yy_action */
#define acttab_yyaction(X,N)  ((X)->aAction[N].action)

/* The value for the N-th entry in yy_lookahead */
#define acttab_yylookahead(X,N)  ((X)->aAction[N].lookahead)

/* Free all memory associated with the given acttab */
void acttab_free(acttab *p){
  free( p->aAction );
  free( p->aLookahead );
  free( p );
}

/* Allocate a new acttab structure */
acttab *acttab_alloc(void){
  acttab *p = (acttab *) calloc( 1, sizeof(*p) );
  if( p==0 ){
    fprintf(stderr,"Unable to allocate memory for a new acttab.");
    exit(1);
  }
  memset(p, 0, sizeof(*p));
  return p;
}

/* Add a new action to the current transaction set.
**
** This routine is called once for each lookahead for a particular
** state.
*/
void acttab_action(acttab *p, int lookahead, int action){
  if( p->nLookahead>=p->nLookaheadAlloc ){
    p->nLookaheadAlloc += 25;
    p->aLookahead = (struct lookahead_action *) realloc( p->aLookahead,
                             sizeof(p->aLookahead[0])*p->nLookaheadAlloc );
    if( p->aLookahead==0 ){
      fprintf(stderr,"malloc failed\n");
      exit(1);
    }
  }
  if( p->nLookahead==0 ){
    p->mxLookahead = lookahead;
    p->mnLookahead = lookahead;
    p->mnAction = action;
  }else{
    if( p->mxLookahead<lookahead ) p->mxLookahead = lookahead;
    if( p->mnLookahead>lookahead ){
      p->mnLookahead = lookahead;
      p->mnAction = action;
    }
  }
  p->aLookahead[p->nLookahead].lookahead = lookahead;
  p->aLookahead[p->nLookahead].action = action;
  p->nLookahead++;
}

/*
** Add the transaction set built up with prior calls to acttab_action()
** into the current action table.  Then reset the transaction set back
** to an empty set in preparation for a new round of acttab_action() calls.
**
** Return the offset into the action table of the new transaction.
*/
int acttab_insert(acttab *p){
  int i, j, k, n;
  assert( p->nLookahead>0 );

  /* Make sure we have enough space to hold the expanded action table
  ** in the worst case.  The worst case occurs if the transaction set
  ** must be appended to the current action table
  */
  n = p->mxLookahead + 1;
  if( p->nAction + n >= p->nActionAlloc ){
    int oldAlloc = p->nActionAlloc;
    p->nActionAlloc = p->nAction + n + p->nActionAlloc + 20;
    p->aAction = (struct lookahead_action *) realloc( p->aAction,
                          sizeof(p->aAction[0])*p->nActionAlloc);
    if( p->aAction==0 ){
      fprintf(stderr,"malloc failed\n");
      exit(1);
    }
    for(i=oldAlloc; i<p->nActionAlloc; i++){
      p->aAction[i].lookahead = -1;
      p->aAction[i].action = -1;
    }
  }

  /* Scan the existing action table looking for an offset that is a
  ** duplicate of the current transaction set.  Fall out of the loop
  ** if and when the duplicate is found.
  **
  ** i is the index in p->aAction[] where p->mnLookahead is inserted.
  */
  for(i=p->nAction-1; i>=0; i--){
    if( p->aAction[i].lookahead==p->mnLookahead ){
      /* All lookaheads and actions in the aLookahead[] transaction
      ** must match against the candidate aAction[i] entry. */
      if( p->aAction[i].action!=p->mnAction ) continue;
      for(j=0; j<p->nLookahead; j++){
        k = p->aLookahead[j].lookahead - p->mnLookahead + i;
        if( k<0 || k>=p->nAction ) break;
        if( p->aLookahead[j].lookahead!=p->aAction[k].lookahead ) break;
        if( p->aLookahead[j].action!=p->aAction[k].action ) break;
      }
      if( j<p->nLookahead ) continue;

      /* No possible lookahead value that is not in the aLookahead[]
      ** transaction is allowed to match aAction[i] */
      n = 0;
      for(j=0; j<p->nAction; j++){
        if( p->aAction[j].lookahead<0 ) continue;
        if( p->aAction[j].lookahead==j+p->mnLookahead-i ) n++;
      }
      if( n==p->nLookahead ){
        break;  /* An exact match is found at offset i */
      }
    }
  }

  /* If no existing offsets exactly match the current transaction, find an
  ** an empty offset in the aAction[] table in which we can add the
  ** aLookahead[] transaction.
  */
  if( i<0 ){
    /* Look for holes in the aAction[] table that fit the current
    ** aLookahead[] transaction.  Leave i set to the offset of the hole.
    ** If no holes are found, i is left at p->nAction, which means the
    ** transaction will be appended. */
    for(i=0; i<p->nActionAlloc - p->mxLookahead; i++){
      if( p->aAction[i].lookahead<0 ){
        for(j=0; j<p->nLookahead; j++){
          k = p->aLookahead[j].lookahead - p->mnLookahead + i;
          if( k<0 ) break;
          if( p->aAction[k].lookahead>=0 ) break;
        }
        if( j<p->nLookahead ) continue;
        for(j=0; j<p->nAction; j++){
          if( p->aAction[j].lookahead==j+p->mnLookahead-i ) break;
        }
        if( j==p->nAction ){
          break;  /* Fits in empty slots */
        }
      }
    }
  }
  /* Insert transaction set at index i. */
  for(j=0; j<p->nLookahead; j++){
    k = p->aLookahead[j].lookahead - p->mnLookahead + i;
    p->aAction[k] = p->aLookahead[j];
    if( k>=p->nAction ) p->nAction = k+1;
  }
  p->nLookahead = 0;

  /* Return the offset that is added to the lookahead in order to get the
  ** index into yy_action of the action */
  return i - p->mnLookahead;
}

/********************** From the file "build.c" *****************************/
/*
** Routines to construction the finite state machine for the LEMON
** parser generator.
*/

/* Find a precedence symbol of every rule in the grammar.
**
** Those rules which have a precedence symbol coded in the input
** grammar using the "[symbol]" construct will already have the
** rp->precsym field filled.  Other rules take as their precedence
** symbol the first RHS symbol with a defined precedence.  If there
** are not RHS symbols with a defined precedence, the precedence
** symbol field is left blank.
*/
void FindRulePrecedences(struct lemon *xp)
{
  struct rule *rp;
  for(rp=xp->rule; rp; rp=rp->next){
    if( rp->precsym==0 ){
      int i, j;
      for(i=0; i<rp->nrhs && rp->precsym==0; i++){
        struct symbol *sp = rp->rhs[i];
        if( sp->type==MULTITERMINAL ){
          for(j=0; j<sp->nsubsym; j++){
            if( sp->subsym[j]->prec>=0 ){
              rp->precsym = sp->subsym[j];
              break;
            }
          }
        }else if( sp->prec>=0 ){
          rp->precsym = rp->rhs[i];
        }
      }
    }
  }
  return;
}

/* Find all nonterminals which will generate the empty string.
** Then go back and compute the first sets of every nonterminal.
** The first set is the set of all terminal symbols which can begin
** a string generated by that nonterminal.
*/
void FindFirstSets(struct lemon *lemp)
{
  int i, j;
  struct rule *rp;
  int progress;

  for(i=0; i<lemp->nsymbol; i++){
    lemp->symbols[i]->lambda = LEMON_FALSE;
  }
  for(i=lemp->nterminal; i<lemp->nsymbol; i++){
    lemp->symbols[i]->firstset = SetNew();
  }

  /* First compute all lambdas */
  do{
    progress = 0;
    for(rp=lemp->rule; rp; rp=rp->next){
      if( rp->lhs->lambda ) continue;
      for(i=0; i<rp->nrhs; i++){
        struct symbol *sp = rp->rhs[i];
        assert( sp->type==NONTERMINAL || sp->lambda==LEMON_FALSE );
        if( sp->lambda==LEMON_FALSE ) break;
      }
      if( i==rp->nrhs ){
        rp->lhs->lambda = LEMON_TRUE;
        progress = 1;
      }
    }
  }while( progress );

  /* Now compute all first sets */
  do{
    struct symbol *s1, *s2;
    progress = 0;
    for(rp=lemp->rule; rp; rp=rp->next){
      s1 = rp->lhs;
      for(i=0; i<rp->nrhs; i++){
        s2 = rp->rhs[i];
        if( s2->type==TERMINAL ){
          progress += SetAdd(s1->firstset,s2->index);
          break;
        }else if( s2->type==MULTITERMINAL ){
          for(j=0; j<s2->nsubsym; j++){
            progress += SetAdd(s1->firstset,s2->subsym[j]->index);
          }
          break;
        }else if( s1==s2 ){
          if( s1->lambda==LEMON_FALSE ) break;
        }else{
          progress += SetUnion(s1->firstset,s2->firstset);
          if( s2->lambda==LEMON_FALSE ) break;
        }
      }
    }
  }while( progress );
  return;
}

/* Compute all LR(0) states for the grammar.  Links
** are added to between some states so that the LR(1) follow sets
** can be computed later.
*/
PRIVATE struct state *getstate(struct lemon *);  /* forward reference */
void FindStates(struct lemon *lemp)
{
  struct symbol *sp;
  struct rule *rp;

  Configlist_init();

  /* Find the start symbol */
  if( lemp->start ){
    sp = Symbol_find(lemp->start);
    if( sp==0 ){
      ErrorMsg(lemp->filename,0,
"The specified start symbol \"%s\" is not \
in a nonterminal of the grammar.  \"%s\" will be used as the start \
symbol instead.",lemp->start,lemp->startRule->lhs->name);
      lemp->errorcnt++;
      sp = lemp->startRule->lhs;
    }
  }else{
    sp = lemp->startRule->lhs;
  }

  /* Make sure the start symbol doesn't occur on the right-hand side of
  ** any rule.  Report an error if it does.  (YACC would generate a new
  ** start symbol in this case.) */
  for(rp=lemp->rule; rp; rp=rp->next){
    int i;
    for(i=0; i<rp->nrhs; i++){
      if( rp->rhs[i]==sp ){   /* FIX ME:  Deal with multiterminals */
        ErrorMsg(lemp->filename,0,
"The start symbol \"%s\" occurs on the \
right-hand side of a rule. This will result in a parser which \
does not work properly.",sp->name);
        lemp->errorcnt++;
      }
    }
  }

  /* The basis configuration set for the first state
  ** is all rules which have the start symbol as their
  ** left-hand side */
  for(rp=sp->rule; rp; rp=rp->nextlhs){
    struct config *newcfp;
    rp->lhsStart = 1;
    newcfp = Configlist_addbasis(rp,0);
    SetAdd(newcfp->fws,0);
  }

  /* Compute the first state.  All other states will be
  ** computed automatically during the computation of the first one.
  ** The returned pointer to the first state is not used. */
  (void)getstate(lemp);
  return;
}

/* Return a pointer to a state which is described by the configuration
** list which has been built from calls to Configlist_add.
*/
PRIVATE void buildshifts(struct lemon *, struct state *); /* Forwd ref */
PRIVATE struct state *getstate(struct lemon *lemp)
{
  struct config *cfp, *bp;
  struct state *stp;

  /* Extract the sorted basis of the new state.  The basis was constructed
  ** by prior calls to "Configlist_addbasis()". */
  Configlist_sortbasis();
  bp = Configlist_basis();

  /* Get a state with the same basis */
  stp = State_find(bp);
  if( stp ){
    /* A state with the same basis already exists!  Copy all the follow-set
    ** propagation links from the state under construction into the
    ** preexisting state, then return a pointer to the preexisting state */
    struct config *x, *y;
    for(x=bp, y=stp->bp; x && y; x=x->bp, y=y->bp){
      Plink_copy(&y->bplp,x->bplp);
      Plink_delete(x->fplp);
      x->fplp = x->bplp = 0;
    }
    cfp = Configlist_return();
    Configlist_eat(cfp);
  }else{
    /* This really is a new state.  Construct all the details */
    Configlist_closure(lemp);    /* Compute the configuration closure */
    Configlist_sort();           /* Sort the configuration closure */
    cfp = Configlist_return();   /* Get a pointer to the config list */
    stp = State_new();           /* A new state structure */
    MemoryCheck(stp);
    stp->bp = bp;                /* Remember the configuration basis */
    stp->cfp = cfp;              /* Remember the configuration closure */
    stp->statenum = lemp->nstate++; /* Every state gets a sequence number */
    stp->ap = 0;                 /* No actions, yet. */
    State_insert(stp,stp->bp);   /* Add to the state table */
    buildshifts(lemp,stp);       /* Recursively compute successor states */
  }
  return stp;
}

/*
** Return true if two symbols are the same.
*/
int same_symbol(struct symbol *a, struct symbol *b)
{
  int i;
  if( a==b ) return 1;
  if( a->type!=MULTITERMINAL ) return 0;
  if( b->type!=MULTITERMINAL ) return 0;
  if( a->nsubsym!=b->nsubsym ) return 0;
  for(i=0; i<a->nsubsym; i++){
    if( a->subsym[i]!=b->subsym[i] ) return 0;
  }
  return 1;
}

/* Construct all successor states to the given state.  A "successor"
** state is any state which can be reached by a shift action.
*/
PRIVATE void buildshifts(struct lemon *lemp, struct state *stp)
{
  struct config *cfp;  /* For looping thru the config closure of "stp" */
  struct config *bcfp; /* For the inner loop on config closure of "stp" */
  struct config *newcfg;  /* */
  struct symbol *sp;   /* Symbol following the dot in configuration "cfp" */
  struct symbol *bsp;  /* Symbol following the dot in configuration "bcfp" */
  struct state *newstp; /* A pointer to a successor state */

  /* Each configuration becomes complete after it contibutes to a successor
  ** state.  Initially, all configurations are incomplete */
  for(cfp=stp->cfp; cfp; cfp=cfp->next) cfp->status = INCOMPLETE;

  /* Loop through all configurations of the state "stp" */
  for(cfp=stp->cfp; cfp; cfp=cfp->next){
    if( cfp->status==COMPLETE ) continue;    /* Already used by inner loop */
    if( cfp->dot>=cfp->rp->nrhs ) continue;  /* Can't shift this config */
    Configlist_reset();                      /* Reset the new config set */
    sp = cfp->rp->rhs[cfp->dot];             /* Symbol after the dot */

    /* For every configuration in the state "stp" which has the symbol "sp"
    ** following its dot, add the same configuration to the basis set under
    ** construction but with the dot shifted one symbol to the right. */
    for(bcfp=cfp; bcfp; bcfp=bcfp->next){
      if( bcfp->status==COMPLETE ) continue;    /* Already used */
      if( bcfp->dot>=bcfp->rp->nrhs ) continue; /* Can't shift this one */
      bsp = bcfp->rp->rhs[bcfp->dot];           /* Get symbol after dot */
      if( !same_symbol(bsp,sp) ) continue;      /* Must be same as for "cfp" */
      bcfp->status = COMPLETE;                  /* Mark this config as used */
      newcfg = Configlist_addbasis(bcfp->rp,bcfp->dot+1);
      Plink_add(&newcfg->bplp,bcfp);
    }

    /* Get a pointer to the state described by the basis configuration set
    ** constructed in the preceding loop */
    newstp = getstate(lemp);

    /* The state "newstp" is reached from the state "stp" by a shift action
    ** on the symbol "sp" */
    if( sp->type==MULTITERMINAL ){
      int i;
      for(i=0; i<sp->nsubsym; i++){
        Action_add(&stp->ap,SHIFT,sp->subsym[i],(char*)newstp);
      }
    }else{
      Action_add(&stp->ap,SHIFT,sp,(char *)newstp);
    }
  }
}

/*
** Construct the propagation links
*/
void FindLinks(struct lemon *lemp)
{
  int i;
  struct config *cfp, *other;
  struct state *stp;
  struct plink *plp;

  /* Housekeeping detail:
  ** Add to every propagate link a pointer back to the state to
  ** which the link is attached. */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      cfp->stp = stp;
    }
  }

  /* Convert all backlinks into forward links.  Only the forward
  ** links are used in the follow-set computation. */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      for(plp=cfp->bplp; plp; plp=plp->next){
        other = plp->cfp;
        Plink_add(&other->fplp,cfp);
      }
    }
  }
}

/* Compute all followsets.
**
** A followset is the set of all symbols which can come immediately
** after a configuration.
*/
void FindFollowSets(struct lemon *lemp)
{
  int i;
  struct config *cfp;
  struct plink *plp;
  int progress;
  int change;

  for(i=0; i<lemp->nstate; i++){
    for(cfp=lemp->sorted[i]->cfp; cfp; cfp=cfp->next){
      cfp->status = INCOMPLETE;
    }
  }

  do{
    progress = 0;
    for(i=0; i<lemp->nstate; i++){
      for(cfp=lemp->sorted[i]->cfp; cfp; cfp=cfp->next){
        if( cfp->status==COMPLETE ) continue;
        for(plp=cfp->fplp; plp; plp=plp->next){
          change = SetUnion(plp->cfp->fws,cfp->fws);
          if( change ){
            plp->cfp->status = INCOMPLETE;
            progress = 1;
          }
        }
        cfp->status = COMPLETE;
      }
    }
  }while( progress );
}

static int resolve_conflict(struct action *,struct action *);

/* Compute the reduce actions, and resolve conflicts.
*/
void FindActions(struct lemon *lemp)
{
  int i,j;
  struct config *cfp;
  struct state *stp;
  struct symbol *sp;
  struct rule *rp;

  /* Add all of the reduce actions
  ** A reduce action is added for each element of the followset of
  ** a configuration which has its dot at the extreme right.
  */
  for(i=0; i<lemp->nstate; i++){   /* Loop over all states */
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){  /* Loop over all configurations */
      if( cfp->rp->nrhs==cfp->dot ){        /* Is dot at extreme right? */
        for(j=0; j<lemp->nterminal; j++){
          if( SetFind(cfp->fws,j) ){
            /* Add a reduce action to the state "stp" which will reduce by the
            ** rule "cfp->rp" if the lookahead symbol is "lemp->symbols[j]" */
            Action_add(&stp->ap,REDUCE,lemp->symbols[j],(char *)cfp->rp);
          }
        }
      }
    }
  }

  /* Add the accepting token */
  if( lemp->start ){
    sp = Symbol_find(lemp->start);
    if( sp==0 ) sp = lemp->startRule->lhs;
  }else{
    sp = lemp->startRule->lhs;
  }
  /* Add to the first state (which is always the starting state of the
  ** finite state machine) an action to ACCEPT if the lookahead is the
  ** start nonterminal.  */
  Action_add(&lemp->sorted[0]->ap,ACCEPT,sp,0);

  /* Resolve conflicts */
  for(i=0; i<lemp->nstate; i++){
    struct action *ap, *nap;
    stp = lemp->sorted[i];
    /* assert( stp->ap ); */
    stp->ap = Action_sort(stp->ap);
    for(ap=stp->ap; ap && ap->next; ap=ap->next){
      for(nap=ap->next; nap && nap->sp==ap->sp; nap=nap->next){
         /* The two actions "ap" and "nap" have the same lookahead.
         ** Figure out which one should be used */
         lemp->nconflict += resolve_conflict(ap,nap);
      }
    }
  }

  /* Report an error for each rule that can never be reduced. */
  for(rp=lemp->rule; rp; rp=rp->next) rp->canReduce = LEMON_FALSE;
  for(i=0; i<lemp->nstate; i++){
    struct action *ap;
    for(ap=lemp->sorted[i]->ap; ap; ap=ap->next){
      if( ap->type==REDUCE ) ap->x.rp->canReduce = LEMON_TRUE;
    }
  }
  for(rp=lemp->rule; rp; rp=rp->next){
    if( rp->canReduce ) continue;
    ErrorMsg(lemp->filename,rp->ruleline,"This rule can not be reduced.\n");
    lemp->errorcnt++;
  }
}

/* Resolve a conflict between the two given actions.  If the
** conflict can't be resolved, return non-zero.
**
** NO LONGER TRUE:
**   To resolve a conflict, first look to see if either action
**   is on an error rule.  In that case, take the action which
**   is not associated with the error rule.  If neither or both
**   actions are associated with an error rule, then try to
**   use precedence to resolve the conflict.
**
** If either action is a SHIFT, then it must be apx.  This
** function won't work if apx->type==REDUCE and apy->type==SHIFT.
*/
static int resolve_conflict(
  struct action *apx,
  struct action *apy
){
  struct symbol *spx, *spy;
  int errcnt = 0;
  assert( apx->sp==apy->sp );  /* Otherwise there would be no conflict */
  if( apx->type==SHIFT && apy->type==SHIFT ){
    apy->type = SSCONFLICT;
    errcnt++;
  }
  if( apx->type==SHIFT && apy->type==REDUCE ){
    spx = apx->sp;
    spy = apy->x.rp->precsym;
    if( spy==0 || spx->prec<0 || spy->prec<0 ){
      /* Not enough precedence information. */
      apy->type = SRCONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){    /* higher precedence wins */
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = SH_RESOLVED;
    }else if( spx->prec==spy->prec && spx->assoc==RIGHT ){ /* Use operator */
      apy->type = RD_RESOLVED;                             /* associativity */
    }else if( spx->prec==spy->prec && spx->assoc==LEFT ){  /* to break tie */
      apx->type = SH_RESOLVED;
    }else{
      assert( spx->prec==spy->prec && spx->assoc==NONE );
      apx->type = ERROR;
    }
  }else if( apx->type==REDUCE && apy->type==REDUCE ){
    spx = apx->x.rp->precsym;
    spy = apy->x.rp->precsym;
    if( spx==0 || spy==0 || spx->prec<0 ||
    spy->prec<0 || spx->prec==spy->prec ){
      apy->type = RRCONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = RD_RESOLVED;
    }
  }else{
    assert(
      apx->type==SH_RESOLVED ||
      apx->type==RD_RESOLVED ||
      apx->type==SSCONFLICT ||
      apx->type==SRCONFLICT ||
      apx->type==RRCONFLICT ||
      apy->type==SH_RESOLVED ||
      apy->type==RD_RESOLVED ||
      apy->type==SSCONFLICT ||
      apy->type==SRCONFLICT ||
      apy->type==RRCONFLICT
    );
    /* The REDUCE/SHIFT case cannot happen because SHIFTs come before
    ** REDUCEs on the list.  If we reach this point it must be because
    ** the parser conflict had already been resolved. */
  }
  return errcnt;
}
/********************* From the file "configlist.c" *************************/
/*
** Routines to processing a configuration list and building a state
** in the LEMON parser generator.
*/

static struct config *freelist = 0;      /* List of free configurations */
static struct config *current = 0;       /* Top of list of configurations */
static struct config **currentend = 0;   /* Last on list of configs */
static struct config *basis = 0;         /* Top of list of basis configs */
static struct config **basisend = 0;     /* End of list of basis configs */

/* Return a pointer to a new configuration */
PRIVATE struct config *newconfig(void){
  struct config *newcfg;
  if( freelist==0 ){
    int i;
    int amt = 3;
    freelist = (struct config *)calloc( amt, sizeof(struct config) );
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new configuration.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  newcfg = freelist;
  freelist = freelist->next;
  return newcfg;
}

/* The configuration "old" is no longer used */
PRIVATE void deleteconfig(struct config *old)
{
  old->next = freelist;
  freelist = old;
}

/* Initialized the configuration list builder */
void Configlist_init(void){
  current = 0;
  currentend = &current;
  basis = 0;
  basisend = &basis;
  Configtable_init();
  return;
}

/* Initialized the configuration list builder */
void Configlist_reset(void){
  current = 0;
  currentend = &current;
  basis = 0;
  basisend = &basis;
  Configtable_clear(0);
  return;
}

/* Add another configuration to the configuration list */
struct config *Configlist_add(
  struct rule *rp,    /* The rule */
  int dot             /* Index into the RHS of the rule where the dot goes */
){
  struct config *cfp, model;

  assert( currentend!=0 );
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){
    cfp = newconfig();
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew();
    cfp->stp = 0;
    cfp->fplp = cfp->bplp = 0;
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp;
    currentend = &cfp->next;
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Add a basis configuration to the configuration list */
struct config *Configlist_addbasis(struct rule *rp, int dot)
{
  struct config *cfp, model;

  assert( basisend!=0 );
  assert( currentend!=0 );
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){
    cfp = newconfig();
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew();
    cfp->stp = 0;
    cfp->fplp = cfp->bplp = 0;
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp;
    currentend = &cfp->next;
    *basisend = cfp;
    basisend = &cfp->bp;
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Compute the closure of the configuration list */
void Configlist_closure(struct lemon *lemp)
{
  struct config *cfp, *newcfp;
  struct rule *rp, *newrp;
  struct symbol *sp, *xsp;
  int i, dot;

  assert( currentend!=0 );
  for(cfp=current; cfp; cfp=cfp->next){
    rp = cfp->rp;
    dot = cfp->dot;
    if( dot>=rp->nrhs ) continue;
    sp = rp->rhs[dot];
    if( sp->type==NONTERMINAL ){
      if( sp->rule==0){
        ErrorMsg(lemp->filename,rp->line,"Nonterminal \"%s\" has no rules.",
          sp->name);
        lemp->errorcnt++;
      }
      for(newrp=sp->rule; newrp; newrp=newrp->nextlhs){
        newcfp = Configlist_add(newrp,0);
        for(i=dot+1; i<rp->nrhs; i++){
          xsp = rp->rhs[i];
          if( xsp->type==TERMINAL ){
            SetAdd(newcfp->fws,xsp->index);
            break;
          }else if( xsp->type==MULTITERMINAL ){
            int k;
            for(k=0; k<xsp->nsubsym; k++){
              SetAdd(newcfp->fws, xsp->subsym[k]->index);
            }
            break;
          }else{
            SetUnion(newcfp->fws,xsp->firstset);
            if( xsp->lambda==LEMON_FALSE ) break;
          }
        }
        if( i==rp->nrhs ) Plink_add(&cfp->fplp,newcfp);
      }
    }
  }
  return;
}

/* Sort the configuration list */
void Configlist_sort(void){
  current = (struct config*)msort((char*)current,(char**)&(current->next),
                                  Configcmp);
  currentend = 0;
  return;
}

/* Sort the basis configuration list */
void Configlist_sortbasis(void){
  basis = (struct config*)msort((char*)current,(char**)&(current->bp),
                                Configcmp);
  basisend = 0;
  return;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_return(void){
  struct config *old;
  old = current;
  current = 0;
  currentend = 0;
  return old;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_basis(void){
  struct config *old;
  old = basis;
  basis = 0;
  basisend = 0;
  return old;
}

/* Free all elements of the given configuration list */
void Configlist_eat(struct config *cfp)
{
  struct config *nextcfp;
  for(; cfp; cfp=nextcfp){
    nextcfp = cfp->next;
    assert( cfp->fplp==0 );
    assert( cfp->bplp==0 );
    if( cfp->fws ) SetFree(cfp->fws);
    deleteconfig(cfp);
  }
  return;
}
/***************** From the file "error.c" *********************************/
/*
** Code for printing error message.
*/

void ErrorMsg(const char *filename, int lineno, const char *format, ...){
  va_list ap;
  fprintf(stderr, "%s:%d: ", filename, lineno);
  va_start(ap, format);
  vfprintf(stderr,format,ap);
  va_end(ap);
  fprintf(stderr, "\n");
}
/**************** From the file "main.c" ************************************/
/*
** Main program file for the LEMON parser generator.
*/

/* Report an out-of-memory condition and abort.  This function
** is used mostly by the "MemoryCheck" macro in struct.h
*/
void memory_error(void){
  fprintf(stderr,"Out of memory.  Aborting...\n");
  exit(1);
}

static char *user_outputcodefilename = NULL;
static void handle_O_option(char *z){
  user_outputcodefilename = (char *) malloc( lemonStrlen(z)+1 );
  if( user_outputcodefilename==0 ){
    memory_error();
  }
  lemon_strcpy(user_outputcodefilename, z);
}

/* Merge together to lists of rules ordered by rule.iRule */
static struct rule *Rule_merge(struct rule *pA, struct rule *pB){
  struct rule *pFirst = 0;
  struct rule **ppPrev = &pFirst;
  while( pA && pB ){
    if( pA->iRule<pB->iRule ){
      *ppPrev = pA;
      ppPrev = &pA->next;
      pA = pA->next;
    }else{
      *ppPrev = pB;
      ppPrev = &pB->next;
      pB = pB->next;
    }
  }
  if( pA ){
    *ppPrev = pA;
  }else{
    *ppPrev = pB;
  }
  return pFirst;
}

/*
** Sort a list of rules in order of increasing iRule value
*/
static struct rule *Rule_sort(struct rule *rp){
  int i;
  struct rule *pNext;
  struct rule *x[32];
  memset(x, 0, sizeof(x));
  while( rp ){
    pNext = rp->next;
    rp->next = 0;
    for(i=0; i<sizeof(x)/sizeof(x[0]) && x[i]; i++){
      rp = Rule_merge(x[i], rp);
      x[i] = 0;
    }
    x[i] = rp;
    rp = pNext;
  }
  rp = 0;
  for(i=0; i<sizeof(x)/sizeof(x[0]); i++){
    rp = Rule_merge(x[i], rp);
  }
  return rp;
}

/* forward reference */
static const char *minimum_size_type(int lwr, int upr, int *pnByte);

/* Print a single line of the "Parser Stats" output
*/
static void stats_line(const char *zLabel, int iValue){
  int nLabel = lemonStrlen(zLabel);
  printf("  %s%.*s %5d\n", zLabel,
         35-nLabel, "................................",
         iValue);
}

/* The main program.  Parse the command line and do it... */
int main(int argc, char **argv)
{
  static int version = 0;
  static int help = 0;
  static int rpflag = 0;
  static int basisflag = 0;
  static int compress = 0;
  static int succeedWithConflicts = 0;
  static int printReport = 0;
  static int statistics = 0;
  static int noResort = 0;
  static struct s_options options[] = {
    {OPT_FSTR, "o", (char*)handle_O_option, "Specify an output file."},
    {OPT_FLAG, "b", (char*)&basisflag, "Print only the basis in report."},
    {OPT_FLAG, "c", (char*)&compress, "Don't compress the action table."},
    {OPT_FLAG, "i", (char*)&succeedWithConflicts, "Return success even in case there are conflicts."},
    {OPT_FLAG, "g", (char*)&rpflag, "Print grammar without actions."},
    {OPT_FLAG, "p", (char*)&showPrecedenceConflict,
                    "Show conflicts resolved by precedence rules"},
    {OPT_FLAG, "r", (char*)&printReport, "Print the report file."},
    {OPT_FLAG, "x", (char*)&noResort, "Do not sort or renumber states"},
    {OPT_FLAG, "s", (char*)&statistics,
                                   "Print parser stats to standard output."},
    {OPT_FLAG, "v", (char*)&version, "Print the version number."},
    {OPT_FLAG, "h", (char*)&help, "Print available command line options."},
    {OPT_FLAG,0,0,0}
  };
  int i;
  int exitcode;
  struct lemon lem;
  struct rule *rp;
  char *input_filename = 0;

  OptInit(argv,argc,options,&input_filename,stderr);
  if( version ){
     printf("Citron version 2.0\n");
     exit(0);
  }
  if( help ){
    fprintf(stderr,"Citron parser generator for Swift\n");
    fprintf(stderr,"Usage: %s [options] <grammar_file>\n", argv[0]);
    fprintf(stderr,"Options:\n");
    OptPrint();
    exit(1);
     exit(0);
  }
  if( input_filename==0 ){
    fprintf(stderr,"Expecting the filename containing the input grammar as a command line argument.\n");
    exit(1);
  }
  memset(&lem, 0, sizeof(lem));
  lem.errorcnt = 0;

  /* Initialize the machine */
  Strsafe_init();
  Symbol_init();
  State_init();
  lem.argv0 = argv[0];
  assert(input_filename);
  lem.filename = input_filename;
  lem.basisflag = basisflag;
  Symbol_new("$");

  /* Parse the input file */
  Parse(&lem);
  if( lem.errorcnt ) exit(lem.errorcnt);
  if( lem.nrule==0 ){
    fprintf(stderr,"Empty grammar.\n");
    exit(1);
  }

  /* Count and index the symbols of the grammar */
  Symbol_new("{default}");
  lem.nsymbol = Symbol_count();
  lem.symbols = Symbol_arrayof();
  for(i=0; i<lem.nsymbol; i++) lem.symbols[i]->index = i;
  qsort(lem.symbols,lem.nsymbol,sizeof(struct symbol*), Symbolcmpp);
  for(i=0; i<lem.nsymbol; i++) lem.symbols[i]->index = i;
  while( lem.symbols[i-1]->type==MULTITERMINAL ){ i--; }
  assert( strcmp(lem.symbols[i-1]->name,"{default}")==0 );
  lem.nsymbol = i - 1;
  for(i=1; ISUPPER(lem.symbols[i]->name[0]); i++);
  lem.nterminal = i;

  /* Assign sequential rule numbers.  Start with 0.  Put rules that have no
  ** reduce action C-code associated with them last, so that the switch()
  ** statement that selects reduction actions will have a smaller jump table.
  */
  for(i=0, rp=lem.rule; rp; rp=rp->next){
    rp->iRule = rp->code ? i++ : -1;
  }
  for(rp=lem.rule; rp; rp=rp->next){
    if( rp->iRule<0 ) rp->iRule = i++;
  }
  lem.startRule = lem.rule;
  lem.rule = Rule_sort(lem.rule);

  /* Generate a reprint of the grammar, if requested on the command line */
  if( rpflag ){
    Reprint(&lem);
  }else{
    /* Initialize the size for all follow and first sets */
    SetSize(lem.nterminal+1);

    /* Find the precedence for every production rule (that has one) */
    FindRulePrecedences(&lem);

    /* Compute the lambda-nonterminals and the first-sets for every
    ** nonterminal */
    FindFirstSets(&lem);

    /* Compute all LR(0) states.  Also record follow-set propagation
    ** links so that the follow-set can be computed later */
    lem.nstate = 0;
    FindStates(&lem);
    lem.sorted = State_arrayof();

    /* Tie up loose ends on the propagation links */
    FindLinks(&lem);

    /* Compute the follow set of every reducible configuration */
    FindFollowSets(&lem);

    /* Compute the action tables */
    FindActions(&lem);

    /* Compress the action tables */
    if( compress==0 ) CompressTables(&lem);

    /* Reorder and renumber the states so that states with fewer choices
    ** occur at the end.  This is an optimization that helps make the
    ** generated parser tables smaller. */
    if( noResort==0 ) ResortStates(&lem);

    /* Generate a report of the parser generated.  (the "y.output" file) */
    if( printReport ) ReportOutput(&lem);

    /* Make sure all symbols have type definitions. Error out otherwise. */
    CheckTypeDefinitions(&lem);

    /* Make sure all rules have code blocks. Error out otherwise. */
    CheckCodeBlocks(&lem);

    /* Make sure %capture_errors directives are clean */
    CheckErrorCaptureDirectives(&lem);

    /* Generate the source code for the parser */
    if( lem.errorcnt==0 ) {
      ReportTable(&lem);
    }
  }
  if( statistics ){
    printf("Parser statistics:\n");
    stats_line("terminal symbols", lem.nterminal);
    stats_line("non-terminal symbols", lem.nsymbol - lem.nterminal);
    stats_line("total symbols", lem.nsymbol);
    stats_line("rules", lem.nrule);
    stats_line("states", lem.nxstate);
    stats_line("conflicts", lem.nconflict);
    if ( lem.errorcnt==0 ) {
      stats_line("action table entries", lem.nactiontab);
      stats_line("total table size (bytes)", lem.tablesize);
    }
  }
  if( lem.nconflict > 0 ){
    fprintf(stderr,"%d parsing conflicts.\n",lem.nconflict);
  }

  /* return 0 on success, 1 on failure. */
  exitcode = 0;
  if (lem.errorcnt > 0) {
    exitcode = 1;
  }
  if (lem.nconflict > 0 && !succeedWithConflicts) {
    exitcode = 1;
  }
  exit(exitcode);
  return (exitcode);
}
/******************** From the file "msort.c" *******************************/
/*
** A generic merge-sort program.
**
** USAGE:
** Let "ptr" be a pointer to some structure which is at the head of
** a null-terminated list.  Then to sort the list call:
**
**     ptr = msort(ptr,&(ptr->next),cmpfnc);
**
** In the above, "cmpfnc" is a pointer to a function which compares
** two instances of the structure and returns an integer, as in
** strcmp.  The second argument is a pointer to the pointer to the
** second element of the linked list.  This address is used to compute
** the offset to the "next" field within the structure.  The offset to
** the "next" field must be constant for all structures in the list.
**
** The function returns a new pointer which is the head of the list
** after sorting.
**
** ALGORITHM:
** Merge-sort.
*/

/*
** Return a pointer to the next structure in the linked list.
*/
#define NEXT(A) (*(char**)(((char*)A)+offset))

/*
** Inputs:
**   a:       A sorted, null-terminated linked list.  (May be null).
**   b:       A sorted, null-terminated linked list.  (May be null).
**   cmp:     A pointer to the comparison function.
**   offset:  Offset in the structure to the "next" field.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   of both a and b.
**
** Side effects:
**   The "next" pointers for elements in the lists a and b are
**   changed.
*/
static char *merge(
  char *a,
  char *b,
  int (*cmp)(const char*,const char*),
  int offset
){
  char *ptr, *head;

  if( a==0 ){
    head = b;
  }else if( b==0 ){
    head = a;
  }else{
    if( (*cmp)(a,b)<=0 ){
      ptr = a;
      a = NEXT(a);
    }else{
      ptr = b;
      b = NEXT(b);
    }
    head = ptr;
    while( a && b ){
      if( (*cmp)(a,b)<=0 ){
        NEXT(ptr) = a;
        ptr = a;
        a = NEXT(a);
      }else{
        NEXT(ptr) = b;
        ptr = b;
        b = NEXT(b);
      }
    }
    if( a ) NEXT(ptr) = a;
    else    NEXT(ptr) = b;
  }
  return head;
}

/*
** Inputs:
**   list:      Pointer to a singly-linked list of structures.
**   next:      Pointer to pointer to the second element of the list.
**   cmp:       A comparison function.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   orginally in list.
**
** Side effects:
**   The "next" pointers for elements in list are changed.
*/
#define LISTSIZE 30
static char *msort(
  char *list,
  char **next,
  int (*cmp)(const char*,const char*)
){
  unsigned long offset;
  char *ep;
  char *set[LISTSIZE];
  int i;
  offset = (unsigned long)((char*)next - (char*)list);
  for(i=0; i<LISTSIZE; i++) set[i] = 0;
  while( list ){
    ep = list;
    list = NEXT(list);
    NEXT(ep) = 0;
    for(i=0; i<LISTSIZE-1 && set[i]!=0; i++){
      ep = merge(ep,set[i],cmp,offset);
      set[i] = 0;
    }
    set[i] = ep;
  }
  ep = 0;
  for(i=0; i<LISTSIZE; i++) if( set[i] ) ep = merge(set[i],ep,cmp,offset);
  return ep;
}
/************************ From the file "option.c" **************************/
static char **argv;
static int argc;
static struct s_options *op;
static FILE *errstream;

#define ISOPT(X) ((X)[0]=='-'||(X)[0]=='+'||strchr((X),'=')!=0)

/*
** Print the command line with a carrot pointing to the k-th character
** of the n-th field.
*/
static void errline(int n, int k, FILE *err)
{
  int spcnt, i;
  if( argv[0] ) fprintf(err,"%s",argv[0]);
  spcnt = lemonStrlen(argv[0]) + 1;
  for(i=1; i<n && argv[i]; i++){
    fprintf(err," %s",argv[i]);
    spcnt += lemonStrlen(argv[i])+1;
  }
  spcnt += k;
  for(; argv[i]; i++) fprintf(err," %s",argv[i]);
  if( spcnt<20 ){
    fprintf(err,"\n%*s^-- here\n",spcnt,"");
  }else{
    fprintf(err,"\n%*shere --^\n",spcnt-7,"");
  }
}

/*
** Return the index of the N-th non-switch argument.  Return -1
** if N is out of range.
*/
static int argindex(int n)
{
  int i;
  int dashdash = 0;
  if( argv!=0 && *argv!=0 ){
    for(i=1; argv[i]; i++){
      if( dashdash || !ISOPT(argv[i]) ){
        if( n==0 ) return i;
        n--;
      }
      if( strcmp(argv[i],"--")==0 ) dashdash = 1;
    }
  }
  return -1;
}

static char emsg[] = "Command line syntax error: ";

/*
** Process a flag command line argument.
*/
static int handleflags(int i, FILE *err, int *skipnext)
{
  int v;
  int errcnt = 0;
  int j;
  for(j=0; op[j].label; j++){
    if( strncmp(&argv[i][1],op[j].label,lemonStrlen(op[j].label))==0 ) break;
  }
  v = argv[i][0]=='-' ? 1 : 0;
  if( op[j].label==0 ){
    if( err ){
      fprintf(err,"%sundefined option.\n",emsg);
      errline(i,1,err);
    }
    errcnt++;
  }else if( op[j].arg==0 ){
    /* Ignore this option */
  }else if( op[j].type==OPT_FLAG ){
    *((int*)op[j].arg) = v;
  }else if( op[j].type==OPT_FFLAG ){
    (*(void(*)(int))(op[j].arg))(v);
  }else if( op[j].type==OPT_FSTR ){
    char *string_value = &argv[i][2]; // E.g.: Skip "-x" in "-xarg"
    if((string_value==0 || string_value[0]==0) && ((i+1)<argc)) {
      string_value = argv[i+1];
      (*skipnext) = 1;
    }else{
      if( err ){
        fprintf(err,"%smissing argument on switch.\n",emsg);
        errline(i,1,err);
      }
    errcnt++;
    }
    (*(void(*)(char *))(op[j].arg))(string_value);
  }else{
    if( err ){
      fprintf(err,"%smissing argument on switch.\n",emsg);
      errline(i,1,err);
    }
    errcnt++;
  }
  return errcnt;
}

/*
** Process a command line switch which has an argument.
*/
static int handleswitch(int i, FILE *err)
{
  int lv = 0;
  double dv = 0.0;
  char *sv = 0, *end;
  char *cp;
  int j;
  int errcnt = 0;
  cp = strchr(argv[i],'=');
  assert( cp!=0 );
  *cp = 0;
  for(j=0; op[j].label; j++){
    if( strcmp(argv[i],op[j].label)==0 ) break;
  }
  *cp = '=';
  if( op[j].label==0 ){
    if( err ){
      fprintf(err,"%sundefined option.\n",emsg);
      errline(i,0,err);
    }
    errcnt++;
  }else{
    cp++;
    switch( op[j].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        if( err ){
          fprintf(err,"%soption requires an argument.\n",emsg);
          errline(i,0,err);
        }
        errcnt++;
        break;
      case OPT_DBL:
      case OPT_FDBL:
        dv = strtod(cp,&end);
        if( *end ){
          if( err ){
            fprintf(err,
               "%sillegal character in floating-point argument.\n",emsg);
            errline(i,(int)((char*)end-(char*)argv[i]),err);
          }
          errcnt++;
        }
        break;
      case OPT_INT:
      case OPT_FINT:
        lv = strtol(cp,&end,0);
        if( *end ){
          if( err ){
            fprintf(err,"%sillegal character in integer argument.\n",emsg);
            errline(i,(int)((char*)end-(char*)argv[i]),err);
          }
          errcnt++;
        }
        break;
      case OPT_STR:
      case OPT_FSTR:
        sv = cp;
        break;
    }
    switch( op[j].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        break;
      case OPT_DBL:
        *(double*)(op[j].arg) = dv;
        break;
      case OPT_FDBL:
        (*(void(*)(double))(op[j].arg))(dv);
        break;
      case OPT_INT:
        *(int*)(op[j].arg) = lv;
        break;
      case OPT_FINT:
        (*(void(*)(int))(op[j].arg))((int)lv);
        break;
      case OPT_STR:
        *(char**)(op[j].arg) = sv;
        break;
      case OPT_FSTR:
        (*(void(*)(char *))(op[j].arg))(sv);
        break;
    }
  }
  return errcnt;
}

int OptInit(char **a, int ac, struct s_options *o, char **input_filename, FILE *err)
{
  int errcnt = 0;
  int filenamecnt = 0;
  argv = a;
  argc = ac;
  op = o;
  errstream = err;
  if( argv && *argv && op ){
    int i;
    for(i=1; argv[i]; i++){
      if( argv[i][0]=='+' || argv[i][0]=='-' ){
        int skipnext = 0;
        errcnt += handleflags(i,err,&skipnext);
        if (skipnext) { i++; }
      }else if( strchr(argv[i],'=') ){
        errcnt += handleswitch(i,err);
      }else{
        if (filenamecnt==0) {
          (*input_filename) = argv[i];
        }
        filenamecnt++;
      }
    }
  }
  if( errcnt>0 ){
    fprintf(err,"Valid command line options for \"%s\" are:\n",*a);
    OptPrint();
    exit(1);
  }
  if( filenamecnt>1 ){
    fprintf(err,"Expecting just one filename as a command line argument, got %d.\n", filenamecnt);
    exit(1);
  }
  return 0;
}

void OptPrint(void){
  int i;
  int max, len;
  max = 0;
  for(i=0; op[i].label; i++){
    len = lemonStrlen(op[i].label) + 1;
    switch( op[i].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        break;
      case OPT_INT:
      case OPT_FINT:
        len += 9;       /* length of "<integer>" */
        break;
      case OPT_DBL:
      case OPT_FDBL:
        len += 6;       /* length of "<real>" */
        break;
      case OPT_STR:
      case OPT_FSTR:
        len += 8;       /* length of "<string>" */
        break;
    }
    if( len>max ) max = len;
  }
  for(i=0; op[i].label; i++){
    switch( op[i].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        fprintf(errstream,"  -%-*s  %s\n",max,op[i].label,op[i].message);
        break;
      case OPT_INT:
      case OPT_FINT:
        fprintf(errstream,"  -%s<integer>%*s  %s\n",op[i].label,
          (int)(max-lemonStrlen(op[i].label)-9),"",op[i].message);
        break;
      case OPT_DBL:
      case OPT_FDBL:
        fprintf(errstream,"  -%s<real>%*s  %s\n",op[i].label,
          (int)(max-lemonStrlen(op[i].label)-6),"",op[i].message);
        break;
      case OPT_STR:
      case OPT_FSTR:
        fprintf(errstream,"  -%s<string>%*s  %s\n",op[i].label,
          (int)(max-lemonStrlen(op[i].label)-8),"",op[i].message);
        break;
    }
  }
}
/*********************** From the file "parse.c" ****************************/
/*
** Input file parser for the LEMON parser generator.
*/

/* The state of the parser */
enum e_state {
  INITIALIZE,
  WAITING_FOR_DECL_OR_RULE,
  WAITING_FOR_DECL_KEYWORD,
  WAITING_FOR_DECL_ARG,
  WAITING_FOR_PRECEDENCE_SYMBOL,
  WAITING_FOR_ARROW,
  IN_RHS,
  RHS_ALIAS_1,
  RHS_ALIAS_2,
  PRECEDENCE_MARK_1,
  PRECEDENCE_MARK_2,
  RESYNC_AFTER_RULE_ERROR,
  RESYNC_AFTER_DECL_ERROR,
  WAITING_FOR_DATATYPE_SYMBOL,
  WAITING_FOR_FALLBACK_ID,
  WAITING_FOR_WILDCARD_ID,
  WAITING_FOR_CLASS_ID,
  WAITING_FOR_CLASS_TOKEN,
  WAITING_FOR_TOKEN_NAME,
  WAITING_FOR_CAPTURE_ERROR_DATATYPE_SYMBOL,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_OPEN,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS_SEPARATOR,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST,
  WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST_SEPARATOR
};
struct pstate {
  char *filename;       /* Name of the input file */
  int tokenlineno;      /* Linenumber at which current token starts */
  int errorcnt;         /* Number of errors so far */
  char *tokenstart;     /* Text of current token */
  struct lemon *gp;     /* Global state vector */
  enum e_state state;        /* The state of the parser */
  struct symbol *fallback;   /* The fallback token */
  struct symbol *tkclass;    /* Token class symbol */
  struct symbol *lhs;        /* Left-hand side of current rule */
  const char *lhsalias;      /* Alias for the LHS */
  int nrhs;                  /* Number of right-hand side symbols seen */
  struct symbol *rhs[MAXRHS];  /* RHS symbols */
  const char *alias[MAXRHS]; /* Aliases for each RHS symbol (or NULL) */
  struct rule *prevrule;     /* Previous rule parsed */
  struct symbol *prevnonterminaltype;   /* Previous nonterminal_type parsed */
  int is_prev_decl_default_nonterminal_type; /* True if the previous parsed construct
                                                was %default_nonterminal_type */
  struct symbol *prevnonterminalcaptureerrors;   /* nonterminal from current %capture_errors */
  int is_in_error_capture_end_after_clause;
  struct symbol* error_capture_tokens[MAX_ERR_CAPTURE_TOKENS];
  int num_error_capture_tokens;
  struct token_sequence error_capture_token_sequences[MAX_ERR_CAPTURE_TOKEN_SEQUENCES];
  int num_error_capture_token_sequences;
  const char *declkeyword;   /* Keyword of a declaration */
  char **declargslot;        /* Where the declaration argument should be put */
  int insertLineMacro;       /* Add #line before declaration insert */
  int *decllinenoslot;       /* Where to write declaration line number */
  enum e_assoc declassoc;    /* Assign this association to decl arguments */
  int preccounter;           /* Assign this precedence to decl arguments */
  struct rule *firstrule;    /* Pointer to first rule in the grammar */
  struct rule *lastrule;     /* Pointer to the most recently parsed rule */
};

/* Parse a single token */
static void parseonetoken(struct pstate *psp)
{
  const char *x;
  x = Strsafe(psp->tokenstart);     /* Save the token permanently */
#if 0
  printf("%s:%d: Token=[%s] state=%d\n",psp->filename,psp->tokenlineno,
    x,psp->state);
#endif
  switch( psp->state ){
    case INITIALIZE:
      psp->prevrule = 0;
      psp->preccounter = 0;
      psp->firstrule = psp->lastrule = 0;
      psp->gp->nrule = 0;
      psp->prevnonterminaltype = 0;
      psp->is_prev_decl_default_nonterminal_type = 0;
      /* Fall thru to next case */
    case WAITING_FOR_DECL_OR_RULE:
      if( x[0]=='%' ){
        psp->state = WAITING_FOR_DECL_KEYWORD;
      }else if( ISLOWER(x[0]) ){
        psp->lhs = Symbol_new(x);
        psp->nrhs = 0;
        psp->lhsalias = 0;
        psp->state = WAITING_FOR_ARROW;
      }else if( x[0]=='{' ){
        if( psp->prevrule==0 && psp->prevnonterminaltype == 0 && psp->is_prev_decl_default_nonterminal_type == 0){
          ErrorMsg(psp->filename,psp->tokenlineno,
"There is no prior rule upon which to attach the code \
block which begins on this line.");
          psp->errorcnt++;
        } else if (psp->prevrule != 0) {
            if (psp->prevrule->code!=0){
              ErrorMsg(psp->filename,psp->tokenlineno,
    "Code block beginning on this line is not the first \
    to follow the previous rule.");
              psp->errorcnt++;
            } else {
              psp->prevrule->line = psp->tokenlineno;
              psp->prevrule->code = &x[1];
              psp->prevrule->noCode = 0;
            }
        } else if (psp->prevnonterminaltype != 0) {
            if (psp->prevnonterminaltype->code!=0){
              ErrorMsg(psp->filename,psp->tokenlineno,
    "Code block beginning on this line is not the first \
    to follow the nonterminal_type declaration.");
              psp->errorcnt++;
            } else {
              psp->prevnonterminaltype->code = &x[1];
              psp->prevnonterminaltype->codeLineNumber = psp->tokenlineno;
            }
            psp->prevnonterminaltype = 0;
        } else if (psp->is_prev_decl_default_nonterminal_type != 0) {
            if (psp->gp->vartype!=0){
                if (psp->gp->defaultCodeBlock!=0){
                  ErrorMsg(psp->filename,psp->tokenlineno,
        "Code block beginning on this line is not the first \
        to follow the default_nonterminal_type declaration.");
                  psp->errorcnt++;
                } else {
                  psp->gp->defaultCodeBlock = &x[1];
                  psp->gp->defaultCodeBlockLineNumber = psp->tokenlineno;
                }
            }
            psp->is_prev_decl_default_nonterminal_type = 0;
        }
      }else if( x[0]=='[' ){
        psp->state = PRECEDENCE_MARK_1;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Token \"%s\" should be either \"%%\" or a nonterminal name.",
          x);
        psp->errorcnt++;
      }
      break;
    case PRECEDENCE_MARK_1:
      if( !ISUPPER(x[0]) ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "The precedence symbol must be a terminal.");
        psp->errorcnt++;
      }else if( psp->prevrule==0 ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "There is no prior rule to assign precedence \"[%s]\".",x);
        psp->errorcnt++;
      }else if( psp->prevrule->precsym!=0 ){
        ErrorMsg(psp->filename,psp->tokenlineno,
"Precedence mark on this line is not the first \
to follow the previous rule.");
        psp->errorcnt++;
      }else{
        psp->prevrule->precsym = Symbol_new(x);
      }
      psp->state = PRECEDENCE_MARK_2;
      break;
    case PRECEDENCE_MARK_2:
      if( x[0]!=']' ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Missing \"]\" on precedence mark.");
        psp->errorcnt++;
      }
      psp->state = WAITING_FOR_DECL_OR_RULE;
      break;
    case WAITING_FOR_ARROW:
      if( x[0]==':' && x[1]==':' && x[2]=='=' ){
        psp->state = IN_RHS;
      }else if( x[0]=='(' ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Expected to see a \":\" following the LHS symbol \"%s\", but got \"(\". LHS symbols don't have aliases.",
          psp->lhs->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Expected to see a \":\" following the LHS symbol \"%s\".",
          psp->lhs->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case IN_RHS:
      if( x[0]=='.' ){
        struct rule *rp;
        rp = (struct rule *)calloc( sizeof(struct rule) +
             sizeof(struct symbol*)*psp->nrhs + sizeof(char*)*psp->nrhs, 1);
        if( rp==0 ){
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Can't allocate enough memory for this rule.");
          psp->errorcnt++;
          psp->prevrule = 0;
        }else{
          int i;
          rp->ruleline = psp->tokenlineno;
          rp->rhs = (struct symbol**)&rp[1];
          rp->rhsalias = (const char**)&(rp->rhs[psp->nrhs]);
          for(i=0; i<psp->nrhs; i++){
            rp->rhs[i] = psp->rhs[i];
            rp->rhsalias[i] = psp->alias[i];
          }
          rp->lhs = psp->lhs;
          rp->lhsalias = psp->lhsalias;
          rp->nrhs = psp->nrhs;
          rp->code = 0;
          rp->noCode = 1;
          rp->precsym = 0;
          rp->index = psp->gp->nrule++;
          rp->nextlhs = rp->lhs->rule;
          rp->lhs->rule = rp;
          rp->next = 0;
          if( psp->firstrule==0 ){
            psp->firstrule = psp->lastrule = rp;
          }else{
            psp->lastrule->next = rp;
            psp->lastrule = rp;
          }
          psp->prevrule = rp;
        }
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( ISALPHA(x[0]) ){
        if( psp->nrhs>=MAXRHS ){
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Too many symbols on RHS of rule beginning at \"%s\".",
            x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_RULE_ERROR;
        }else{
          psp->rhs[psp->nrhs] = Symbol_new(x);
          psp->alias[psp->nrhs] = 0;
          psp->nrhs++;
        }
      }else if( (x[0]=='|' || x[0]=='/') && psp->nrhs>0 ){
        struct symbol *msp = psp->rhs[psp->nrhs-1];
        if( msp->type!=MULTITERMINAL ){
          struct symbol *origsp = msp;
          msp = (struct symbol *) calloc(1,sizeof(*msp));
          memset(msp, 0, sizeof(*msp));
          msp->type = MULTITERMINAL;
          msp->nsubsym = 1;
          msp->subsym = (struct symbol **) calloc(1,sizeof(struct symbol*));
          msp->subsym[0] = origsp;
          msp->name = origsp->name;
          psp->rhs[psp->nrhs-1] = msp;
        }
        msp->nsubsym++;
        msp->subsym = (struct symbol **) realloc(msp->subsym,
          sizeof(struct symbol*)*msp->nsubsym);
        msp->subsym[msp->nsubsym-1] = Symbol_new(&x[1]);
        if( ISLOWER(x[1]) || ISLOWER(msp->subsym[0]->name[0]) ){
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Cannot form a compound containing a non-terminal");
          psp->errorcnt++;
        }
      }else if( x[0]=='(' && psp->nrhs>0 ){
        psp->state = RHS_ALIAS_1;
      }else if( x[0]==':' || x[0]=='%' ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Illegal character on RHS of rule: \"%s\" (Did you forget to finish the previous rule with a '.'?).",x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Illegal character on RHS of rule: \"%s\".",x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case RHS_ALIAS_1:
      if( ISALPHA(x[0]) ){
        psp->alias[psp->nrhs-1] = x;
        psp->state = RHS_ALIAS_2;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "\"%s\" is not a valid alias for the RHS symbol \"%s\"\n",
          x,psp->rhs[psp->nrhs-1]->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case RHS_ALIAS_2:
      if( x[0]==')' ){
        psp->state = IN_RHS;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Missing \")\" following LHS alias name \"%s\".",psp->lhsalias);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case WAITING_FOR_DECL_KEYWORD:
      if( ISALPHA(x[0]) ){
        psp->declkeyword = x;
        psp->declargslot = 0;
        psp->decllinenoslot = 0;
        psp->insertLineMacro = 1;
        psp->state = WAITING_FOR_DECL_ARG;
        if( strcmp(x,"class_name")==0 ){
          psp->declargslot = &(psp->gp->className);
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"preface")==0 ){
          psp->declargslot = &(psp->gp->preface);
          psp->gp->prefaceLineNumber = psp->tokenlineno;
        }else if( strcmp(x,"epilogue")==0 ){
          psp->declargslot = &(psp->gp->epilogue);
          psp->gp->epilogueLineNumber = psp->tokenlineno;
        }else if( strcmp(x,"tokencode_prefix")==0 ){
          psp->declargslot = &psp->gp->tokenprefix;
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"nonterminalcode_prefix")==0 ){
          psp->declargslot = &psp->gp->nonterminalprefix;
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"extra_class_members")==0 ){
          psp->declargslot = &(psp->gp->extraClassMembers);
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"token_type")==0 ){
          psp->declargslot = &(psp->gp->tokentype);
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"default_nonterminal_type")==0 ){
          psp->declargslot = &(psp->gp->vartype);
          psp->insertLineMacro = 0;
          psp->is_prev_decl_default_nonterminal_type = 1;
        }else if( strcmp(x,"start_symbol")==0 ){
          psp->declargslot = &(psp->gp->start);
          psp->insertLineMacro = 0;
        }else if( strcmp(x,"left_associative")==0 ){
          psp->preccounter++;
          psp->declassoc = LEFT;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
        }else if( strcmp(x,"right_associative")==0 ){
          psp->preccounter++;
          psp->declassoc = RIGHT;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
        }else if( strcmp(x,"nonassociative")==0 ){
          psp->preccounter++;
          psp->declassoc = NONE;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
        }else if( strcmp(x,"nonterminal_type")==0 ){
          psp->state = WAITING_FOR_DATATYPE_SYMBOL;
        }else if( strcmp(x,"fallback")==0 ){
          psp->fallback = 0;
          psp->state = WAITING_FOR_FALLBACK_ID;
        }else if( strcmp(x,"token")==0 ){
          psp->state = WAITING_FOR_TOKEN_NAME;
        }else if( strcmp(x,"wildcard")==0 ){
          psp->state = WAITING_FOR_WILDCARD_ID;
        }else if( strcmp(x,"token_set")==0 ){
          psp->state = WAITING_FOR_CLASS_ID;
        }else if( strcmp(x,"capture_errors")==0 ){
          psp->state = WAITING_FOR_CAPTURE_ERROR_DATATYPE_SYMBOL;
        }else{
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Unknown declaration keyword: \"%%%s\".",x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_DECL_ERROR;
        }
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Illegal declaration keyword: \"%s\".",x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
      break;
    case WAITING_FOR_DATATYPE_SYMBOL:
      if( !ISALPHA(x[0]) ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Symbol name missing after %%nonterminal_type keyword");
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }else{
        struct symbol *sp = Symbol_find(x);
        if((sp) && (sp->datatype)){
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Symbol %%nonterminal_type \"%s\" already defined", x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_DECL_ERROR;
        }else{
          if (!sp){
            sp = Symbol_new(x);
          }
          psp->declargslot = &sp->datatype;
          psp->insertLineMacro = 0;
          psp->prevnonterminaltype = sp;
          psp->state = WAITING_FOR_DECL_ARG;
        }
      }
      break;
    case WAITING_FOR_CAPTURE_ERROR_DATATYPE_SYMBOL:
      if( !ISALPHA(x[0]) ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Symbol name missing after %%capture_errors keyword");
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }else{
        struct symbol *sp = Symbol_find(x);
        if (!sp){
          sp = Symbol_new(x);
        }
        if (sp->error_capture_line > 0) {
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Symbol '%s' already has %%capture_errors defined earlier in line %d", sp->name, sp->error_capture_line);
          psp->errorcnt++;
        }
        sp->error_capture_line = psp->tokenlineno;
        psp->prevnonterminalcaptureerrors = sp;
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE;
      }
      break;
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE:
      if( x[0]=='.' ){
        psp->num_error_capture_tokens = 0;
        psp->num_error_capture_token_sequences = 0;
        psp->state = WAITING_FOR_DECL_OR_RULE;
      } else if (strcmp(x, "end_before") == 0) {
        psp->is_in_error_capture_end_after_clause = 0;
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_OPEN;
      } else if (strcmp(x, "end_after") == 0) {
        psp->is_in_error_capture_end_after_clause = 1;
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_OPEN;
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Expecting an end_before or end_after clause after '%%capture_errors %s'", psp->prevnonterminalcaptureerrors->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
      break;
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_OPEN:
      if( x[0]=='(' ){
        psp->num_error_capture_token_sequences = 0;
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS;
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Expecting \"(\" after \"end_before\" or \"end_after\", got \"%s\"",x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
      break;
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS_SEPARATOR:
      if( x[0]==',' || x[0] == '|' ){
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS;
        break;
      } else if( x[0]==')' ){
        if (psp->num_error_capture_token_sequences > 0) {
          struct symbol *sp = psp->prevnonterminalcaptureerrors;
          struct token_sequence **token_sequences_slot = 0;
          int *num_token_sequences_slot = 0;
          if (psp->is_in_error_capture_end_after_clause) {
            token_sequences_slot = &sp->error_capture_end_after_sequences;
            num_token_sequences_slot = &sp->num_error_capture_end_after_sequences;
          } else {
            token_sequences_slot = &sp->error_capture_end_before_sequences;
            num_token_sequences_slot = &sp->num_error_capture_end_before_sequences;
          }
          if (*token_sequences_slot == 0) {
            (*token_sequences_slot) = (struct token_sequence *) calloc(psp->num_error_capture_token_sequences, sizeof(struct token_sequence));
          } else {
            (*token_sequences_slot) = (struct token_sequence *) realloc((*token_sequences_slot), ((*num_token_sequences_slot) + psp->num_error_capture_token_sequences) * sizeof(struct token_sequence));
          }
          int n = (*num_token_sequences_slot);
          for (int i = 0; i < psp->num_error_capture_token_sequences; i++) {
            (*token_sequences_slot)[n + i] = psp->error_capture_token_sequences[i];
          }
          (*num_token_sequences_slot) = n + psp->num_error_capture_token_sequences;
          psp->gp->has_error_capture = 1;
          psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE;
        }
        break;
      } else if( ISUPPER(x[0]) || x[0] == '[' ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Missing separator ( \",\" or \"|\") between token sequences in \"%s\" clause",
            (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        // fallthrough
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Error in \"%s\" clause",
          (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
        break;
      }
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS:
      if( ISUPPER(x[0]) ){
        if (psp->num_error_capture_token_sequences>=MAX_ERR_CAPTURE_TOKEN_SEQUENCES) {
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Too many token sequences in \"%s\" clause, beginning from token \"%s\".",
            (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"),
            x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_RULE_ERROR;
        } else {
          psp->error_capture_token_sequences[psp->num_error_capture_token_sequences].tokens.single_token = Symbol_new(x);
          psp->error_capture_token_sequences[psp->num_error_capture_token_sequences].count = 1;
          psp->num_error_capture_token_sequences++;
          psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS_SEPARATOR;
        }
      } else if( x[0]=='[' ){
        if (psp->is_in_error_capture_end_after_clause == 0) {
          ErrorMsg(psp->filename,psp->tokenlineno,
            "end_before clause should not contain any token sequences.");
          psp->errorcnt++;
        }
        psp->num_error_capture_tokens = 0;
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST;
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Error in \"%s\" clause",
          (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST_SEPARATOR:
      if( x[0]==','){
        psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST;
        break;
      } else if( x[0]==']' ){
        int token_count = psp->num_error_capture_tokens;
        if (token_count > 0) {
          if (token_count == 1) {
            psp->error_capture_token_sequences[psp->num_error_capture_token_sequences].tokens.single_token = psp->error_capture_tokens[0];
          } else {
            struct symbol **mt = (struct symbol **) calloc(token_count, sizeof(struct symbol *));
            for (int i = 0; i < token_count; i++) {
              mt[i] = psp->error_capture_tokens[i];
            }
            psp->error_capture_token_sequences[psp->num_error_capture_token_sequences].tokens.multiple_tokens = mt;
          }
          psp->error_capture_token_sequences[psp->num_error_capture_token_sequences].count = token_count;
          psp->num_error_capture_token_sequences++;
          psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKENS_SEPARATOR;
        }
        break;
      } else if( ISUPPER(x[0]) ){
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Missing separator ( \",\" ) between tokens in list in \"%s\" clause",
            (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        // fallthrough
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Error in \"%s\" clause",
          (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
        break;
      }
    case WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST:
      if( ISUPPER(x[0]) ){
        if (psp->num_error_capture_tokens>=MAX_ERR_CAPTURE_TOKENS) {
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Too many tokens in sequence in \"%s\" clause, beginning from token \"%s\".",
            (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"),
            x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_RULE_ERROR;
        } else {
          psp->error_capture_tokens[psp->num_error_capture_tokens] = Symbol_new(x);
          psp->num_error_capture_tokens++;
          psp->state = WAITING_FOR_CAPTURE_ERROR_END_CLAUSE_TOKEN_LIST_SEPARATOR;
        }
      } else {
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Error in \"%s\" clause",
          (psp->is_in_error_capture_end_after_clause ? "end_after" : "end_before"));
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
      break;
    case WAITING_FOR_PRECEDENCE_SYMBOL:
      if( x[0]=='.' ){
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( ISUPPER(x[0]) ){
        struct symbol *sp;
        sp = Symbol_new(x);
        if( sp->prec>=0 ){
          ErrorMsg(psp->filename,psp->tokenlineno,
            "Symbol \"%s\" has already be given a precedence.",x);
          psp->errorcnt++;
        }else{
          sp->prec = psp->preccounter;
          sp->assoc = psp->declassoc;
        }
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Can't assign a precedence to \"%s\".",x);
        psp->errorcnt++;
      }
      break;
    case WAITING_FOR_DECL_ARG:
      if( x[0]=='{' || x[0]=='\"' || ISALNUM(x[0]) ){
        const char *zOld, *zNew;
        char *zBuf, *z;
        int nOld, n, nLine = 0, nNew, nBack;
        char zLine[50];
        zNew = x;
        if( zNew[0]=='"' || zNew[0]=='{' ) zNew++;
        nNew = lemonStrlen(zNew);
        if( *psp->declargslot ){
          zOld = *psp->declargslot;
        }else{
          zOld = "";
        }
        nOld = lemonStrlen(zOld);
        n = nOld + nNew + 20;
        *psp->declargslot = (char *) realloc(*psp->declargslot, n);
        zBuf = *psp->declargslot + nOld;
        if( psp->decllinenoslot && psp->decllinenoslot[0]==0 ){
          psp->decllinenoslot[0] = psp->tokenlineno;
        }
        memcpy(zBuf, zNew, nNew);
        zBuf += nNew;
        *zBuf = 0;
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else{
        ErrorMsg(psp->filename,psp->tokenlineno,
          "Illegal argument to %%%s: %s",psp->declkeyword,x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
      break;
    case WAITING_FOR_FALLBACK_ID:
      if( x[0]=='.' ){
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( !ISUPPER(x[0]) ){
        ErrorMsg(psp->filename, psp->tokenlineno,
          "%%fallback argument \"%s\" should be a token", x);
        psp->errorcnt++;
      }else{
        struct symbol *sp = Symbol_new(x);
        if( psp->fallback==0 ){
          psp->fallback = sp;
        }else if( sp->fallback ){
          ErrorMsg(psp->filename, psp->tokenlineno,
            "More than one fallback assigned to token %s", x);
          psp->errorcnt++;
        }else{
          sp->fallback = psp->fallback;
          psp->gp->has_fallback = 1;
        }
      }
      break;
    case WAITING_FOR_TOKEN_NAME:
      /* Tokens do not have to be declared before use.  But they can be
      ** in order to control their assigned integer number.  The number for
      ** each token is assigned when it is first seen.  So by including
      **
      **     %token ONE TWO THREE
      **
      ** early in the grammar file, that assigns small consecutive values
      ** to each of the tokens ONE TWO and THREE.
      */
      if( x[0]=='.' ){
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( !ISUPPER(x[0]) ){
        ErrorMsg(psp->filename, psp->tokenlineno,
          "%%token argument \"%s\" should be a token", x);
        psp->errorcnt++;
      }else{
        (void)Symbol_new(x);
      }
      break;
    case WAITING_FOR_WILDCARD_ID:
      if( x[0]=='.' ){
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( !ISUPPER(x[0]) ){
        ErrorMsg(psp->filename, psp->tokenlineno,
          "%%wildcard argument \"%s\" should be a token", x);
        psp->errorcnt++;
      }else{
        struct symbol *sp = Symbol_new(x);
        if( psp->gp->wildcard==0 ){
          psp->gp->wildcard = sp;
        }else{
          ErrorMsg(psp->filename, psp->tokenlineno,
            "Extra wildcard to token: %s", x);
          psp->errorcnt++;
        }
      }
      break;
    case WAITING_FOR_CLASS_ID:
      if( !ISLOWER(x[0]) ){
        ErrorMsg(psp->filename, psp->tokenlineno,
          "%%token_class must be followed by an identifier: ", x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
     }else if( Symbol_find(x) ){
        ErrorMsg(psp->filename, psp->tokenlineno,
          "Symbol \"%s\" already used", x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }else{
        psp->tkclass = Symbol_new(x);
        psp->tkclass->type = MULTITERMINAL;
        psp->state = WAITING_FOR_CLASS_TOKEN;
      }
      break;
    case WAITING_FOR_CLASS_TOKEN:
      if( x[0]=='.' ){
        psp->state = WAITING_FOR_DECL_OR_RULE;
      }else if( ISUPPER(x[0]) || ((x[0]=='|' || x[0]=='/') && ISUPPER(x[1])) ){
        struct symbol *msp = psp->tkclass;
        msp->nsubsym++;
        msp->subsym = (struct symbol **) realloc(msp->subsym,
          sizeof(struct symbol*)*msp->nsubsym);
        if( !ISUPPER(x[0]) ) x++;
        msp->subsym[msp->nsubsym-1] = Symbol_new(x);
      }else{
        ErrorMsg(psp->filename, psp->tokenlineno,
          "%%token_class argument \"%s\" should be a token", x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
      break;
    case RESYNC_AFTER_RULE_ERROR:
/*      if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
**      break; */
    case RESYNC_AFTER_DECL_ERROR:
      if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
      if( x[0]=='%' ) psp->state = WAITING_FOR_DECL_KEYWORD;
      break;
  }
}

/* In spite of its name, this function is really a scanner.  It read
** in the entire input file (all at once) then tokenizes it.  Each
** token is passed to the function "parseonetoken" which builds all
** the appropriate data structures in the global state vector "gp".
*/
void Parse(struct lemon *gp)
{
  struct pstate ps;
  FILE *fp;
  char *filebuf;
  unsigned int filesize;
  int lineno;
  int c;
  char *cp, *nextcp;
  int startline = 0;

  memset(&ps, '\0', sizeof(ps));
  ps.gp = gp;
  ps.filename = gp->filename;
  ps.errorcnt = 0;
  ps.state = INITIALIZE;

  /* Begin by reading the input file */
  fp = fopen(ps.filename,"rb");
  if( fp==0 ){
    ErrorMsg(ps.filename,0,"Can't open this file for reading.");
    gp->errorcnt++;
    return;
  }
  fseek(fp,0,2);
  filesize = ftell(fp);
  rewind(fp);
  filebuf = (char *)malloc( filesize+1 );
  if( filesize>100000000 || filebuf==0 ){
    ErrorMsg(ps.filename,0,"Input file too large.");
    gp->errorcnt++;
    fclose(fp);
    return;
  }
  if( fread(filebuf,1,filesize,fp)!=filesize ){
    ErrorMsg(ps.filename,0,"Can't read in all %d bytes of this file.",
      filesize);
    free(filebuf);
    gp->errorcnt++;
    fclose(fp);
    return;
  }
  fclose(fp);
  filebuf[filesize] = 0;

  /* Now scan the text of the input file */
  lineno = 1;
  for(cp=filebuf; (c= *cp)!=0; ){
    if( c=='\n' ) lineno++;              /* Keep track of the line number */
    if( ISSPACE(c) ){ cp++; continue; }  /* Skip all white space */
    if( c=='/' && cp[1]=='/' ){          /* Skip C++ style comments */
      cp+=2;
      while( (c= *cp)!=0 && c!='\n' ) cp++;
      continue;
    }
    if( c=='/' && cp[1]=='*' ){          /* Skip C style comments */
      cp+=2;
      while( (c= *cp)!=0 && (c!='/' || cp[-1]!='*') ){
        if( c=='\n' ) lineno++;
        cp++;
      }
      if( c ) cp++;
      continue;
    }
    ps.tokenstart = cp;                /* Mark the beginning of the token */
    ps.tokenlineno = lineno;           /* Linenumber on which token begins */
    if( c=='\"' ){                     /* String literals */
      cp++;
      while( (c= *cp)!=0 && c!='\"' ){
        if( c=='\n' ) lineno++;
        cp++;
      }
      if( c==0 ){
        ErrorMsg(ps.filename,startline,
"String starting on this line is not terminated before the end of the file.");
        ps.errorcnt++;
        nextcp = cp;
      }else{
        nextcp = cp+1;
      }
    }else if( c=='{' ){               /* A block of C code */
      int level;
      cp++;
      for(level=1; (c= *cp)!=0 && (level>1 || c!='}'); cp++){
        if( c=='\n' ) lineno++;
        else if( c=='{' ) level++;
        else if( c=='}' ) level--;
        else if( c=='/' && cp[1]=='*' ){  /* Skip comments */
          int prevc;
          cp = &cp[2];
          prevc = 0;
          while( (c= *cp)!=0 && (c!='/' || prevc!='*') ){
            if( c=='\n' ) lineno++;
            prevc = c;
            cp++;
          }
        }else if( c=='/' && cp[1]=='/' ){  /* Skip C++ style comments too */
          cp = &cp[2];
          while( (c= *cp)!=0 && c!='\n' ) cp++;
          if( c ) lineno++;
        }else if( c=='\'' || c=='\"' ){    /* String a character literals */
          int startchar, prevc;
          startchar = c;
          prevc = 0;
          for(cp++; (c= *cp)!=0 && (c!=startchar || prevc=='\\'); cp++){
            if( c=='\n' ) lineno++;
            if( prevc=='\\' ) prevc = 0;
            else              prevc = c;
          }
        }
      }
      if( c==0 ){
        ErrorMsg(ps.filename,ps.tokenlineno,
"C code starting on this line is not terminated before the end of the file.");
        ps.errorcnt++;
        nextcp = cp;
      }else{
        nextcp = cp+1;
      }
    }else if( ISALNUM(c) ){          /* Identifiers */
      while( (c= *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
      nextcp = cp;
    }else if( c==':' && cp[1]==':' && cp[2]=='=' ){ /* The operator "::=" */
      cp += 3;
      nextcp = cp;
    }else if( (c=='/' || c=='|') && ISALPHA(cp[1]) ){
      cp += 2;
      while( (c = *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
      nextcp = cp;
    }else{                          /* All other (one character) operators */
      cp++;
      nextcp = cp;
    }
    c = *cp;
    *cp = 0;                        /* Null terminate the token */
    parseonetoken(&ps);             /* Parse the token */
    *cp = (char)c;                  /* Restore the buffer */
    cp = nextcp;
  }
  free(filebuf);                    /* Release the buffer after parsing */
  gp->rule = ps.firstrule;
  gp->errorcnt = ps.errorcnt;
}

void CheckTypeDefinitions(struct lemon *lemp) {
  if (lemp->tokentype == 0) {
    ErrorMsg(lemp->filename,0,
"Type for terminals is undefined. Please use %%token_type to define that.");
    lemp->errorcnt++;
  }
  int number_of_nonterminals_with_type_undefined = 0;
  if (lemp->vartype==0) {
    int i = 0;
    for (i = 1 /* Skip the base symbol */; i < lemp->nsymbol; i++) {
      struct symbol *sp = lemp->symbols[i];
      if (sp->type==NONTERMINAL && sp->datatype==0) {
    ErrorMsg(lemp->filename,0,
"Type for nonterminal '%s' is undefined.",
        sp->name);
        number_of_nonterminals_with_type_undefined++;
      }
    }
  }
  if (number_of_nonterminals_with_type_undefined > 0) {
    ErrorMsg(lemp->filename,0,
"Type for one or more nonterminals is undefined. Please use \
%%nonterminal_type or %%default_nonterminal_type to define them.");
    lemp->errorcnt++;
  }
}

void CheckCodeBlocks(struct lemon *lemp) {
  struct rule *rp;
  for(rp=lemp->rule; rp; rp=rp->next) {
    if (rp->code != 0) { continue; } // Rule has code block
    if (rp->lhs->datatype != 0 && rp->lhs->code != 0) { continue; } // Rule's lhs nonterminal has default code block
    if (rp->lhs->datatype == 0 && // Rule's lhs nonterminal doesn't have type definition
        lemp->defaultCodeBlock != 0) { continue; } // and default_nonterminal_type has default code block
  ErrorMsg(lemp->filename, rp->ruleline,
"Rule should be followed by a code block.");
  lemp->errorcnt++;
  }
}

static struct symbol *find_start_symbol(struct lemon *lemp);

void CheckErrorCaptureDirectives(struct lemon *lemp) {
  struct symbol *start_symbol = find_start_symbol(lemp);
  for (int i = 1 /* Skip the base symbol */; i < lemp->nsymbol; i++) {
    struct symbol *sp = lemp->symbols[i];
    if (sp->error_capture_line == 0) { continue; }
    if (sp->type != NONTERMINAL) {
        ErrorMsg(lemp->filename, sp->error_capture_line,
"%%capture_errors can be defined only on non-terminals - '%s' is not a non-terminal.", sp->name);
        lemp->errorcnt++;
    }
    // The start symbol should have no end_before or end_after clauses.
    // Non-start-symbols can have either an end_before or an end_after clause, or both, or neither.
    if (sp == start_symbol) {
      if (sp->num_error_capture_end_before_sequences > 0 || sp->num_error_capture_end_after_sequences > 0) {
        ErrorMsg(lemp->filename, sp->error_capture_line,
"%%capture_errors on start symbol '%s' should not have any 'end_before' or 'end_after' clauses.", sp->name);
        lemp->errorcnt++;
      }
    }
  }
  // Make sure %capture_errors is not defined on any multiterminals
  for(struct rule *rp = lemp->rule; rp; rp = rp->next) {
    for(int i = 0; i < rp->nrhs; i++) {
        struct symbol *sp = rp->rhs[i];
        if (sp->error_capture_line == 0) { continue; }
        if (sp->type != NONTERMINAL) {
            ErrorMsg(lemp->filename, sp->error_capture_line,
    "%%capture_errors can be defined only on non-terminals - '%s' is not a non-terminal.", sp->name);
            lemp->errorcnt++;
        }
    }
  }
}

/*************************** From the file "plink.c" *********************/
/*
** Routines processing configuration follow-set propagation links
** in the LEMON parser generator.
*/
static struct plink *plink_freelist = 0;

/* Allocate a new plink */
struct plink *Plink_new(void){
  struct plink *newlink;

  if( plink_freelist==0 ){
    int i;
    int amt = 100;
    plink_freelist = (struct plink *)calloc( amt, sizeof(struct plink) );
    if( plink_freelist==0 ){
      fprintf(stderr,
      "Unable to allocate memory for a new follow-set propagation link.\n");
      exit(1);
    }
    for(i=0; i<amt-1; i++) plink_freelist[i].next = &plink_freelist[i+1];
    plink_freelist[amt-1].next = 0;
  }
  newlink = plink_freelist;
  plink_freelist = plink_freelist->next;
  return newlink;
}

/* Add a plink to a plink list */
void Plink_add(struct plink **plpp, struct config *cfp)
{
  struct plink *newlink;
  newlink = Plink_new();
  newlink->next = *plpp;
  *plpp = newlink;
  newlink->cfp = cfp;
}

/* Transfer every plink on the list "from" to the list "to" */
void Plink_copy(struct plink **to, struct plink *from)
{
  struct plink *nextpl;
  while( from ){
    nextpl = from->next;
    from->next = *to;
    *to = from;
    from = nextpl;
  }
}

/* Delete every plink on the list */
void Plink_delete(struct plink *plp)
{
  struct plink *nextpl;

  while( plp ){
    nextpl = plp->next;
    plp->next = plink_freelist;
    plink_freelist = plp;
    plp = nextpl;
  }
}
/*********************** From the file "report.c" **************************/
/*
** Procedures for generating reports and tables in the LEMON parser generator.
*/

/* Generate a filename with the given suffix.  Space to hold the
** name comes from malloc() and must be freed by the calling
** function.
*/
PRIVATE char *file_makename(struct lemon *lemp, const char *suffix)
{
  char *name;
  char *cp;

  name = (char*)malloc( lemonStrlen(lemp->filename) + lemonStrlen(suffix) + 5 );
  if( name==0 ){
    fprintf(stderr,"Can't allocate space for a filename.\n");
    exit(1);
  }
  lemon_strcpy(name,lemp->filename);
  cp = strrchr(name,'.');
  if( cp ) *cp = 0;
  lemon_strcat(name,suffix);
  return name;
}

/* Open a file with a name based on the name of the input file,
** but with a different (specified) suffix, and return a pointer
** to the stream */
PRIVATE FILE *file_open(
  struct lemon *lemp,
  const char *suffix,
  const char *mode,
  char *userSpecifiedName
){
  FILE *fp;

  if( lemp->outname ) free(lemp->outname);
  lemp->outname = userSpecifiedName ? userSpecifiedName : file_makename(lemp, suffix);
  fp = fopen(lemp->outname,mode);
  if( fp==0 && *mode=='w' ){
    fprintf(stderr,"Can't open file \"%s\".\n",lemp->outname);
    lemp->errorcnt++;
    return 0;
  }
  return fp;
}

/* Duplicate the input file without comments and without actions
** on rules */
void Reprint(struct lemon *lemp)
{
  struct rule *rp;
  struct symbol *sp;
  int i, j, maxlen, len, ncolumns, skip;
  printf("// Reprint of input file \"%s\".\n// Symbols:\n",lemp->filename);
  maxlen = 10;
  for(i=0; i<lemp->nsymbol; i++){
    sp = lemp->symbols[i];
    len = lemonStrlen(sp->name);
    if( len>maxlen ) maxlen = len;
  }
  ncolumns = 76/(maxlen+5);
  if( ncolumns<1 ) ncolumns = 1;
  skip = (lemp->nsymbol + ncolumns - 1)/ncolumns;
  for(i=0; i<skip; i++){
    printf("//");
    for(j=i; j<lemp->nsymbol; j+=skip){
      sp = lemp->symbols[j];
      assert( sp->index==j );
      printf(" %3d %-*.*s",j,maxlen,maxlen,sp->name);
    }
    printf("\n");
  }
  for(rp=lemp->rule; rp; rp=rp->next){
    printf("%s",rp->lhs->name);
    /*    if( rp->lhsalias ) printf("(%s)",rp->lhsalias); */
    printf(" ::=");
    for(i=0; i<rp->nrhs; i++){
      sp = rp->rhs[i];
      if( sp->type==MULTITERMINAL ){
        printf(" %s", sp->subsym[0]->name);
        for(j=1; j<sp->nsubsym; j++){
          printf("|%s", sp->subsym[j]->name);
        }
      }else{
        printf(" %s", sp->name);
      }
      /* if( rp->rhsalias[i] ) printf("(%s)",rp->rhsalias[i]); */
    }
    printf(".");
    if( rp->precsym ) printf(" [%s]",rp->precsym->name);
    /* if( rp->code ) printf("\n    %s",rp->code); */
    printf("\n");
  }
}

/* Print a single rule.
*/
void RulePrint(FILE *fp, struct rule *rp, int iCursor){
  struct symbol *sp;
  int i, j;
  fprintf(fp,"%s ::=",rp->lhs->name);
  for(i=0; i<=rp->nrhs; i++){
    if( i==iCursor ) fprintf(fp," *");
    if( i==rp->nrhs ) break;
    sp = rp->rhs[i];
    if( sp->type==MULTITERMINAL ){
      fprintf(fp," %s", sp->subsym[0]->name);
      for(j=1; j<sp->nsubsym; j++){
        fprintf(fp,"|%s",sp->subsym[j]->name);
      }
    }else{
      fprintf(fp," %s", sp->name);
    }
  }
}

/* Print the rule for a configuration.
*/
void ConfigPrint(FILE *fp, struct config *cfp){
  RulePrint(fp, cfp->rp, cfp->dot);
}

/* #define TEST */
#if 0
/* Print a set */
PRIVATE void SetPrint(out,set,lemp)
FILE *out;
char *set;
struct lemon *lemp;
{
  int i;
  char *spacer;
  spacer = "";
  fprintf(out,"%12s[","");
  for(i=0; i<lemp->nterminal; i++){
    if( SetFind(set,i) ){
      fprintf(out,"%s%s",spacer,lemp->symbols[i]->name);
      spacer = " ";
    }
  }
  fprintf(out,"]\n");
}

/* Print a plink chain */
PRIVATE void PlinkPrint(out,plp,tag)
FILE *out;
struct plink *plp;
char *tag;
{
  while( plp ){
    fprintf(out,"%12s%s (state %2d) ","",tag,plp->cfp->stp->statenum);
    ConfigPrint(out,plp->cfp);
    fprintf(out,"\n");
    plp = plp->next;
  }
}
#endif

/* Print an action to the given file descriptor.  Return FALSE if
** nothing was actually printed.
*/
int PrintAction(
  struct action *ap,          /* The action to print */
  FILE *fp,                   /* Print the action here */
  int indent                  /* Indent by this amount */
){
  int result = 1;
  switch( ap->type ){
    case SHIFT: {
      struct state *stp = ap->x.stp;
      fprintf(fp,"%*s shift        %-7d",indent,ap->sp->name,stp->statenum);
      break;
    }
    case REDUCE: {
      struct rule *rp = ap->x.rp;
      fprintf(fp,"%*s reduce       %-7d",indent,ap->sp->name,rp->iRule);
      RulePrint(fp, rp, -1);
      break;
    }
    case SHIFTREDUCE: {
      struct rule *rp = ap->x.rp;
      fprintf(fp,"%*s shift-reduce %-7d",indent,ap->sp->name,rp->iRule);
      RulePrint(fp, rp, -1);
      break;
    }
    case ACCEPT:
      fprintf(fp,"%*s accept",indent,ap->sp->name);
      break;
    case ERROR:
      fprintf(fp,"%*s error",indent,ap->sp->name);
      break;
    case SRCONFLICT:
    case RRCONFLICT:
      fprintf(fp,"%*s reduce       %-7d ** Parsing conflict **",
        indent,ap->sp->name,ap->x.rp->iRule);
      break;
    case SSCONFLICT:
      fprintf(fp,"%*s shift        %-7d ** Parsing conflict **",
        indent,ap->sp->name,ap->x.stp->statenum);
      break;
    case SH_RESOLVED:
      if( showPrecedenceConflict ){
        fprintf(fp,"%*s shift        %-7d -- dropped by precedence",
                indent,ap->sp->name,ap->x.stp->statenum);
      }else{
        result = 0;
      }
      break;
    case RD_RESOLVED:
      if( showPrecedenceConflict ){
        fprintf(fp,"%*s reduce %-7d -- dropped by precedence",
                indent,ap->sp->name,ap->x.rp->iRule);
      }else{
        result = 0;
      }
      break;
    case NOT_USED:
      result = 0;
      break;
  }
  if( result && ap->spOpt ){
    fprintf(fp,"  /* because %s==%s */", ap->sp->name, ap->spOpt->name);
  }
  return result;
}

/* Generate the "*.out" log file */
void ReportOutput(struct lemon *lemp)
{
  int i;
  struct state *stp;
  struct config *cfp;
  struct action *ap;
  FILE *fp;

  fp = file_open(lemp,".out","wb", NULL);
  if( fp==0 ) return;
  for(i=0; i<lemp->nxstate; i++){
    stp = lemp->sorted[i];
    fprintf(fp,"State %d:\n",stp->statenum);
    if( lemp->basisflag ) cfp=stp->bp;
    else                  cfp=stp->cfp;
    while( cfp ){
      char buf[20];
      if( cfp->dot==cfp->rp->nrhs ){
        lemon_sprintf(buf,"(%d)",cfp->rp->iRule);
        fprintf(fp,"    %5s ",buf);
      }else{
        fprintf(fp,"          ");
      }
      ConfigPrint(fp,cfp);
      fprintf(fp,"\n");
#if 0
      SetPrint(fp,cfp->fws,lemp);
      PlinkPrint(fp,cfp->fplp,"To  ");
      PlinkPrint(fp,cfp->bplp,"From");
#endif
      if( lemp->basisflag ) cfp=cfp->bp;
      else                  cfp=cfp->next;
    }
    fprintf(fp,"\n");
    for(ap=stp->ap; ap; ap=ap->next){
      if( PrintAction(ap,fp,30) ) fprintf(fp,"\n");
    }
    fprintf(fp,"\n");
  }
  fprintf(fp, "----------------------------------------------------\n");
  fprintf(fp, "Symbols:\n");
  for(i=0; i<lemp->nsymbol; i++){
    int j;
    struct symbol *sp;

    sp = lemp->symbols[i];
    fprintf(fp, "  %3d: %s", i, sp->name);
    if( sp->type==NONTERMINAL ){
      fprintf(fp, ":");
      if( sp->lambda ){
        fprintf(fp, " <lambda>");
      }
      for(j=0; j<lemp->nterminal; j++){
        if( sp->firstset && SetFind(sp->firstset, j) ){
          fprintf(fp, " %s", lemp->symbols[j]->name);
        }
      }
    }
    fprintf(fp, "\n");
  }
  fclose(fp);
  return;
}

/* Search for the file "name" which is in the same directory as
** the exacutable */
PRIVATE char *pathsearch(char *argv0, char *name, int modemask)
{
  const char *pathlist;
  char *pathbufptr;
  char *pathbuf;
  char *path,*cp;
  char c;

#ifdef __WIN32__
  cp = strrchr(argv0,'\\');
#else
  cp = strrchr(argv0,'/');
#endif
  if( cp ){
    c = *cp;
    *cp = 0;
    path = (char *)malloc( lemonStrlen(argv0) + lemonStrlen(name) + 2 );
    if( path ) lemon_sprintf(path,"%s/%s",argv0,name);
    *cp = c;
  }else{
    pathlist = getenv("PATH");
    if( pathlist==0 ) pathlist = ".:/bin:/usr/bin";
    pathbuf = (char *) malloc( lemonStrlen(pathlist) + 1 );
    path = (char *)malloc( lemonStrlen(pathlist)+lemonStrlen(name)+2 );
    if( (pathbuf != 0) && (path!=0) ){
      pathbufptr = pathbuf;
      lemon_strcpy(pathbuf, pathlist);
      while( *pathbuf ){
        cp = strchr(pathbuf,':');
        if( cp==0 ) cp = &pathbuf[lemonStrlen(pathbuf)];
        c = *cp;
        *cp = 0;
        lemon_sprintf(path,"%s/%s",pathbuf,name);
        *cp = c;
        if( c==0 ) pathbuf[0] = 0;
        else pathbuf = &cp[1];
        if( access(path,modemask)==0 ) break;
      }
      free(pathbufptr);
    }
  }
  return path;
}

/* Given an action, compute the integer value for that action
** which is to be put in the action table of the generated machine.
** Return negative if no action should be generated.
*/
PRIVATE int compute_action(struct lemon *lemp, struct action *ap)
{
  int act;
  switch( ap->type ){
    case SHIFT:  act = ap->x.stp->statenum;                        break;
    case SHIFTREDUCE: {
      act = ap->x.rp->iRule + lemp->nstate;
      /* Since a SHIFT is inherient after a prior REDUCE, convert any
      ** SHIFTREDUCE action with a nonterminal on the LHS into a simple
      ** REDUCE action: */
      if( ap->sp->index>=lemp->nterminal ) act += lemp->nrule;
      break;
    }
    case REDUCE: act = ap->x.rp->iRule + lemp->nstate+lemp->nrule; break;
    case ERROR:  act = lemp->nstate + lemp->nrule*2;               break;
    case ACCEPT: act = lemp->nstate + lemp->nrule*2 + 1;           break;
    default:     act = -1; break;
  }
  return act;
}

#define LINESIZE 1000

/*
** Print the definition of the union used for the parser's data stack.
** This union contains fields for every possible data type for tokens
** and nonterminals.  In the process of computing and printing this
** union, also set the ".dtnum" field of every terminal and nonterminal
** symbol.
*/
void print_symbol_enumeration(
  FILE *out,                  /* The output stream */
  struct lemon *lemp         /* The main info structure for this parser */
){
  char **types;             /* A hash table of datatypes */
  int arraysize;            /* Size of the "types" array */
  int maxdtlength;          /* Maximum length of any ".datatype" field. */
  char *stddt;              /* Standardized name for a datatype */
  int i,j;                  /* Loop counters */
  unsigned hash;            /* For hashing the name of a type */

  /* Allocate and initialize types[] and allocate stddt[] */
  arraysize = lemp->nsymbol * 2;
  types = (char**)calloc( arraysize, sizeof(char*) );
  if( types==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }
  for(i=0; i<arraysize; i++) types[i] = 0;
  maxdtlength = 0;
  if( lemp->vartype ){
    maxdtlength = lemonStrlen(lemp->vartype);
  }
  for(i=0; i<lemp->nsymbol; i++){
    int len;
    struct symbol *sp = lemp->symbols[i];
    if( sp->datatype==0 ) continue;
    len = lemonStrlen(sp->datatype);
    if( len>maxdtlength ) maxdtlength = len;
  }
  stddt = (char*)malloc( maxdtlength*2 + 1 );
  if( stddt==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  /* Build a hash table of datatypes. The ".dtnum" field of each symbol
  ** is filled in with the hash index plus 1.  A ".dtnum" value of 0 is
  ** used for terminal symbols.  If there is no %default_type defined then
  ** 0 is also used as the .dtnum value for nonterminals which do not specify
  ** a datatype using the %type directive.
  */
  for(i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    char *cp;
    if( sp->type!=NONTERMINAL || (sp->datatype==0 && lemp->vartype==0) ){
      sp->dtnum = 0;
      continue;
    }
    cp = sp->datatype;
    if( cp==0 ) cp = lemp->vartype;
    j = 0;
    while( ISSPACE(*cp) ) cp++;
    while( *cp ) stddt[j++] = *cp++;
    while( j>0 && ISSPACE(stddt[j-1]) ) j--;
    stddt[j] = 0;
    if( lemp->tokentype && strcmp(stddt, lemp->tokentype)==0 ){
      sp->dtnum = 0;
      continue;
    }
    hash = 0;
    for(j=0; stddt[j]; j++){
      hash = hash*53 + stddt[j];
    }
    hash = (hash & 0x7fffffff)%arraysize;
    while( types[hash] ){
      if( strcmp(types[hash],stddt)==0 ){
        sp->dtnum = hash + 1;
        break;
      }
      hash++;
      if( hash>=(unsigned)arraysize ) hash = 0;
    }
    if( types[hash]==0 ){
      sp->dtnum = hash + 1;
      types[hash] = (char*)malloc( lemonStrlen(stddt)+1 );
      if( types[hash]==0 ){
        fprintf(stderr,"Out of memory.\n");
        exit(1);
      }
      lemon_strcpy(types[hash],stddt);
    }
  }

  /* Print out the definition of Symbol as a "union"-ish enum */
  fprintf(out,"    enum CitronSymbol {\n");
  fprintf(out,"        case yyBaseOfStack\n");
  fprintf(out,"        case yy0(CitronToken)\n");
  for(i=0; i<arraysize; i++){
    if( types[i]==0 ) continue;
    fprintf(out,"        case yy%d(%s)\n",i+1,types[i]);
  }
  fprintf(out,"\n");
  fprintf(out,"        func typeErasedContent() -> Any {\n"); // Used in error capturing
  fprintf(out,"            switch (self) {\n");
  fprintf(out,"            case .yyBaseOfStack: fatalError()\n");
  fprintf(out,"            case .yy0(let value): return value as Any\n");
  for(i=0; i<arraysize; i++){
    if( types[i]==0 ) continue;
    fprintf(out,"            case .yy%d(let value): return value as Any\n",i+1);
  }
  fprintf(out,"            }\n");
  fprintf(out,"        }\n");
  fprintf(out,"    }\n");
  free(stddt);
  for(i=0; i<arraysize; i++){
    free(types[i]);
  }
  free(types);
}

/*
** Return the name of a C datatype able to represent values between
** lwr and upr, inclusive.  If pnByte!=NULL then also write the sizeof
** for that type (1, 2, or 4) into *pnByte.
*/
static const char *minimum_size_type(int lwr, int upr, int *pnByte){
  const char *zType = "Int";
  int nByte = 4;
  if( lwr>=0 ){
    if( upr<=255 ){
      zType = "UInt8";
      nByte = 1;
    }else if( upr<65535 ){
      zType = "UInt16";
      nByte = 2;
    }else{
      zType = "UInt32";
      nByte = 4;
    }
  }else if( lwr>=-127 && upr<=127 ){
    zType = "Int8";
    nByte = 1;
  }else if( lwr>=-32767 && upr<32767 ){
    zType = "Int16";
    nByte = 2;
  }
  if( pnByte ) *pnByte = nByte;
  return zType;
}

/*
** Each state contains a set of token transaction and a set of
** nonterminal transactions.  Each of these sets makes an instance
** of the following structure.  An array of these structures is used
** to order the creation of entries in the yy_action[] table.
*/
struct axset {
  struct state *stp;   /* A pointer to a state */
  int isTkn;           /* True to use tokens.  False for non-terminals */
  int nAction;         /* Number of actions */
  int iOrder;          /* Original order of action sets */
};

/*
** Compare to axset structures for sorting purposes
*/
static int axset_compare(const void *a, const void *b){
  struct axset *p1 = (struct axset*)a;
  struct axset *p2 = (struct axset*)b;
  int c;
  c = p2->nAction - p1->nAction;
  if( c==0 ){
    c = p1->iOrder - p2->iOrder;
  }
  assert( c!=0 || p1==p2 );
  return c;
}

/*
** Write text on "out" that describes the rule "rp".
*/
static void writeRuleText(FILE *out, struct rule *rp){
  int j;
  fprintf(out,"%s ::=", rp->lhs->name);
  for(j=0; j<rp->nrhs; j++){
    struct symbol *sp = rp->rhs[j];
    if( sp->type!=MULTITERMINAL ){
      const char *rhsalias = rp->rhsalias[j];
      if (rhsalias) {
        fprintf(out," %s(%s)", sp->name, rhsalias);
      } else {
        fprintf(out," %s", sp->name);
      }
    }else{
      int k;
      fprintf(out," %s", sp->subsym[0]->name);
      for(k=1; k<sp->nsubsym; k++){
        fprintf(out,"|%s",sp->subsym[k]->name);
      }
    }
  }
}

static const char *type_string_of_symbol(struct symbol *sp, struct lemon *lemp) {
  if (sp->type == TERMINAL) {
    return lemp->tokentype;
  } else if (sp->type == NONTERMINAL) {
    if (sp->datatype==0 && lemp->vartype) {
      return lemp->vartype;
    }
    return sp->datatype;
  } else if (sp->type == MULTITERMINAL) {
    assert(sp->subsym[0]->type == TERMINAL);
    return lemp->tokentype;
  }
  return 0;
}

static int dtnum_of_symbol(struct symbol *sp) {
  if (sp->type == TERMINAL || sp->type == NONTERMINAL) {
      return sp->dtnum;
  } else if (sp->type == MULTITERMINAL) {
      return sp->subsym[0]->dtnum;
  }
  return -1;
}

static struct symbol *find_start_symbol(struct lemon *lemp) {
  struct symbol *sp = 0;
  if (lemp->start) {
    sp = Symbol_find(lemp->start);
    if( sp==0 ) sp = lemp->startRule->lhs;
  } else {
    sp = lemp->startRule->lhs;
  }
  return sp;
}

static void print_swift_file_intro(FILE *out) {
  fprintf(out,
    "// This file is automatically generated by Citron version 2.0.\n"
    "//\n"
    "// The parser class defined below conforms to the CitronParser protocol\n"
    "// defined in CitronParser.swift. \n"
    "// \n"
    "// The authors of Citron disclaim copyright to the source code in this file.\n"
    "\n");
}

void dump_config_info(struct config *cfg) {
    printf("  Rule %d: %s <-", cfg->rp->index, cfg->rp->lhs->name);
    for (int i = 0; i < cfg->rp->nrhs; i++) {
        if (i == cfg->dot) { printf(" ."); }
        printf(" %s", cfg->rp->rhs[i]->name);
    }
    if (cfg->rp->nrhs == cfg->dot) { printf(" ."); }
}

void dump_state_info(int index, struct state *stp) {
    printf("State %d\n", index);
    int i = 1;
    printf("  Basis configs:\n");
    for (struct config *cfg = stp->bp; cfg != 0; cfg = cfg->bp) {
        printf("    %d:", i++);
        dump_config_info(cfg);
        printf("\n");
    }
    i = 1;
    printf("  All configs:\n");
    for (struct config *cfg = stp->cfp; cfg != 0; cfg = cfg->next) {
        printf("    %d:", i++);
        dump_config_info(cfg);
        printf("\n");
    }
}

/* Generate C source code for the parser */
void ReportTable(struct lemon *lemp){
  FILE *out;
  char line[LINESIZE];
  int  lineno;
  struct state *stp;
  struct action *ap;
  struct rule *rp;
  struct acttab *pActtab;
  int i, j, n, sz;
  int szActionType;     /* sizeof(YYACTIONTYPE) */
  int szCodeType;       /* sizeof(YYCODETYPE)   */
  const char *name;
  int mnTknOfst, mxTknOfst;
  int mnNtOfst, mxNtOfst;
  struct axset *ax;

  out = file_open(lemp,".swift","wb", user_outputcodefilename);
  if( out==0 ){
    return;
  }

  print_swift_file_intro(out);

  // Preface

  if (lemp->preface) {
    fprintf(out, "// Preface\n\n");
    fprintf(out, "#sourceLocation(file: \"%s\", line: %d)\n", lemp->filename, lemp->prefaceLineNumber);
    fprintf(out, "%s\n", lemp->preface);
    fprintf(out, "#sourceLocation()\n\n");
  }

  // Open the Parser class

  fprintf(out, "// Parser class\n\n");
  const char *className = lemp->className ? lemp->className : "Parser";
  fprintf(out, "class %s: CitronParser {\n\n", className);

  // Types

  fprintf(out, "    // Types\n\n");

  fprintf(out, "    typealias CitronSymbolNumber = %s\n",
    minimum_size_type(0, lemp->nsymbol+1, &szCodeType));

  fprintf(out, "    typealias CitronStateNumber = %s\n",
    minimum_size_type(0, lemp->nstate, NULL));

  fprintf(out, "    typealias CitronRuleNumber = %s\n\n",
    minimum_size_type(0, lemp->nrule, NULL));

  const char *tokenPrefix = lemp->tokenprefix ? lemp->tokenprefix : "";
  fprintf(out, "    enum CitronTokenCode: CitronSymbolNumber {\n");
  if (lemp->nterminal > 1) {
    for(i=1; i<lemp->nterminal; i++){
      fprintf(out, "      case %s%-30s = %3d\n", tokenPrefix, lemp->symbols[i]->name, i);
    }
  } else {
      fprintf(out, "      case %-30s = %3d\n", "Dummy", 1);
  }
  fprintf(out, "    }\n\n");

  const char *nonterminalPrefix = lemp->nonterminalprefix ? lemp->nonterminalprefix : "";
  fprintf(out, "    enum CitronNonTerminalCode: CitronSymbolNumber {\n");
  for(i=lemp->nterminal; i<lemp->nsymbol; i++){
    fprintf(out, "      case %s%-30s = %3d\n", nonterminalPrefix, lemp->symbols[i]->name, i);
  }
  fprintf(out, "    }\n\n");

  fprintf(out, "    enum CitronSymbolCode : RawRepresentable, Equatable {\n");
  fprintf(out, "        case token(CitronTokenCode)\n");
  fprintf(out, "        case nonterminal(CitronNonTerminalCode)\n");
  fprintf(out, "        case endOfInput\n\n");
  fprintf(out, "        init(_ token: CitronTokenCode) { self = .token(token) }\n");
  fprintf(out, "        init(_ nonterminal: CitronNonTerminalCode) { self = .nonterminal(nonterminal) }\n");
  fprintf(out, "        init(rawValue: CitronSymbolNumber) {\n");
  fprintf(out, "            if (rawValue == 0) {\n");
  fprintf(out, "                self = .endOfInput\n");
  fprintf(out, "            } else if (rawValue < %d) {\n", lemp->nterminal);
  fprintf(out, "                self = .token(CitronTokenCode(rawValue: rawValue)!)\n");
  fprintf(out, "            } else if (rawValue < %d) {\n", lemp->nsymbol);
  fprintf(out, "                self = .nonterminal(CitronNonTerminalCode(rawValue: rawValue)!)\n");
  fprintf(out, "            } else {\n");
  fprintf(out, "                fatalError()\n");
  fprintf(out, "            }\n");
  fprintf(out, "        }\n\n");
  fprintf(out, "        typealias RawValue = CitronSymbolNumber\n");
  fprintf(out, "        var rawValue: CitronSymbolNumber {\n");
  fprintf(out, "            switch (self) {\n");
  fprintf(out, "            case .token(let t): return t.rawValue\n");
  fprintf(out, "            case .nonterminal(let nt): return nt.rawValue\n");
  fprintf(out, "            case .endOfInput: return 0\n");
  fprintf(out, "            }\n");
  fprintf(out, "        }\n");
  fprintf(out, "    }\n\n");

  assert(lemp->tokentype != 0);
  fprintf(out,"    typealias CitronToken = %s\n\n", lemp->tokentype); // %token_type

  print_symbol_enumeration(out, lemp); // Generates "enum CitronSymbol {...}"
  fprintf(out, "\n");

  struct symbol *start_symbol = find_start_symbol(lemp);
  fprintf(out,"    typealias CitronResult = %s\n\n", type_string_of_symbol(start_symbol, lemp));

  // Action tables computation

  ax = (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
  if( ax==0 ){
    fprintf(stderr,"malloc failed\n");
    exit(1);
  }
  for(i=0; i<lemp->nxstate; i++){
    stp = lemp->sorted[i];
    ax[i*2].stp = stp;
    ax[i*2].isTkn = 1;
    ax[i*2].nAction = stp->nTknAct;
    ax[i*2+1].stp = stp;
    ax[i*2+1].isTkn = 0;
    ax[i*2+1].nAction = stp->nNtAct;
    // dump_state_info(i, stp);
  }
  mxTknOfst = mnTknOfst = 0;
  mxNtOfst = mnNtOfst = 0;
  /* In an effort to minimize the action table size, use the heuristic
  ** of placing the largest action sets first */
  for(i=0; i<lemp->nxstate*2; i++) ax[i].iOrder = i;
  qsort(ax, lemp->nxstate*2, sizeof(ax[0]), axset_compare);
  pActtab = acttab_alloc();
  for(i=0; i<lemp->nxstate*2 && ax[i].nAction>0; i++){
    stp = ax[i].stp;
    if( ax[i].isTkn ){
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index>=lemp->nterminal ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iTknOfst = acttab_insert(pActtab);
      if( stp->iTknOfst<mnTknOfst ) mnTknOfst = stp->iTknOfst;
      if( stp->iTknOfst>mxTknOfst ) mxTknOfst = stp->iTknOfst;
    }else{
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index<lemp->nterminal ) continue;
        if( ap->sp->index==lemp->nsymbol ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iNtOfst = acttab_insert(pActtab);
      if( stp->iNtOfst<mnNtOfst ) mnNtOfst = stp->iNtOfst;
      if( stp->iNtOfst>mxNtOfst ) mxNtOfst = stp->iNtOfst;
    }
#if 0  /* Uncomment for a trace of how the yy_action[] table fills out */
    { int jj, nn;
      for(jj=nn=0; jj<pActtab->nAction; jj++){
        if( pActtab->aAction[jj].action<0 ) nn++;
      }
      printf("%4d: State %3d %s n: %2d size: %5d freespace: %d\n",
             i, stp->statenum, ax[i].isTkn ? "Token" : "Var  ",
             ax[i].nAction, pActtab->nAction, nn);
    }
#endif
  }
  free(ax);

  /* Mark rules that are actually used for reduce actions after all
  ** optimizations have been applied
  */
  for(rp=lemp->rule; rp; rp=rp->next) rp->doesReduce = LEMON_FALSE;
  for(i=0; i<lemp->nxstate; i++){
    for(ap=lemp->sorted[i]->ap; ap; ap=ap->next){
      if( ap->type==REDUCE || ap->type==SHIFTREDUCE ){
        ap->x.rp->doesReduce = 1;
      }
    }
  }

  // Counts

  fprintf(out, "    // Counts\n\n");

  fprintf(out, "    let yyNumberOfSymbols: Int = %d\n", lemp->nsymbol);
  fprintf(out, "    let yyNumberOfStates: Int = %d\n\n",lemp->nxstate);

  // Action tables output

  fprintf(out, "    // Action tables\n\n");

  /* Output the yy_lookahead + yy_action table */

  lemp->nactiontab = n = acttab_size(pActtab);
  lemp->tablesize += n*szActionType;
  int shiftMax = lemp->nstate;
  int shiftReduceMax = shiftMax + lemp->nrule;
  int reduceMax = shiftReduceMax + lemp->nrule;
  fprintf(out,"    let yyLookaheadAction: [(CitronSymbolNumber, CitronParsingAction)] = [\n");
  for(i=j=0; i<n; i++){
    int action = acttab_yyaction(pActtab, i);
    if( action<0 ) action = lemp->nstate + lemp->nrule + 2;
    int la = acttab_yylookahead(pActtab, i);
    if( la<0 ) la = lemp->nsymbol;
    if( j==0 ) fprintf(out, "/* %3d */ ", i);
    fprintf(out, " (%2d,", la);
    if (action<shiftMax) { // Shift
      fprintf(out, " .SH(%2d)),", action);
    } else if (action<shiftReduceMax) { // Shift-Reduce
      fprintf(out, " .SR(%2d)),", action - shiftMax);
    } else if (action<reduceMax) { // Reduce
      fprintf(out, " .RD(%2d)),", action - shiftReduceMax);
    } else if (action == reduceMax) {
      fprintf(out, " .ERROR ),  ");
    } else if (action == reduceMax+1) {
      fprintf(out, " .ACCEPT),  ");
    } else {
      printf("Bad action code");
      assert(0);
    }
    if( j==4 || i==n-1 ){
      fprintf(out, "\n");
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "    ]\n\n");

  /* Output the yy_shift_ofst[] table */
  n = lemp->nxstate;
  while( n>0 && lemp->sorted[n-1]->iTknOfst==NO_OFFSET ) n--;
  fprintf(out, "    let yyShiftUseDefault: Int = %d\n", lemp->nactiontab);
  fprintf(out, "    let yyShiftOffsetMin: Int = %d\n", mnTknOfst);
  fprintf(out, "    let yyShiftOffsetMax: Int = %d\n", mxTknOfst);
  fprintf(out, "    let yyShiftOffset: [Int] = [\n");
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iTknOfst;
    if( ofst==NO_OFFSET ) ofst = lemp->nactiontab;
    if( j==0 ) fprintf(out, "        /* %5d */ ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n");
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "    ]\n\n");

  /* Output the yy_reduce_ofst[] table */
  fprintf(out, "    let yyReduceUseDefault: Int = %d\n", mnNtOfst-1);
  n = lemp->nxstate;
  while( n>0 && lemp->sorted[n-1]->iNtOfst==NO_OFFSET ) n--;
  fprintf(out, "    let yyReduceOffsetMin: Int =   %d\n", mnNtOfst);
  fprintf(out, "    let yyReduceOffsetMax: Int =   %d\n", mxNtOfst);
  fprintf(out, "    let yyReduceOffset: [Int] = [\n");
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iNtOfst;
    if( ofst==NO_OFFSET ) ofst = mnNtOfst - 1;
    if( j==0 ) fprintf(out,"        /* %5d */ ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n");
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "    ]\n\n");

  /* Output the default action table */
  fprintf(out, "    let yyDefaultAction: [CitronParsingAction] = [\n");
  n = lemp->nxstate;
  lemp->tablesize += n*szActionType;
  for(i=j=0; i<n; i++){
    stp = lemp->sorted[i];
    if( j==0 ) fprintf(out,"  /* %5d */ ", i);
    if (stp->iDfltReduce<lemp->nrule) {
      fprintf(out, " .RD(%2d),", stp->iDfltReduce);
    } else if (stp->iDfltReduce==lemp->nrule) {
      fprintf(out, " .ERROR ,");
    } else if (stp->iDfltReduce==lemp->nrule+1) {
      fprintf(out, " .ACCEPT,");
    } else {
      printf("Bad rule number");
      assert(0);
    }
    if( j==4 || i==n-1 ){
      fprintf(out, "\n");
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "    ]\n\n");

  // Fallback

  fprintf(out, "    // Fallback\n\n");

  if( lemp->has_fallback ){
    fprintf(out, "    let yyHasFallback: Bool = true\n");
    fprintf(out, "    let yyFallback: [CitronSymbolNumber] = [\n");
    int mx = lemp->nterminal - 1;
    while( mx>0 && lemp->symbols[mx]->fallback==0 ){ mx--; }
    lemp->tablesize += (mx+1)*szCodeType;
    for(i=0; i<=mx; i++){
      struct symbol *p = lemp->symbols[i];
      if( p->fallback==0 ){
        fprintf(out, "        0,  /* %10s => nothing */\n", p->name);
      }else{
        fprintf(out, "      %3d,  /* %10s => %s */\n", p->fallback->index,
          p->name, p->fallback->name);
      }
    }
    fprintf(out, "    ]\n\n");
  } else {
    fprintf(out, "    let yyHasFallback: Bool = false\n");
    fprintf(out, "    let yyFallback: [CitronSymbolNumber] = []\n\n");
  }

  // Wildcard

  fprintf(out, "    // Wildcard\n\n");

  if( lemp->wildcard ) {
    fprintf(out, "    let yyWildcard: CitronSymbolNumber? = %d\n\n", lemp->wildcard->index);
  } else {
    fprintf(out, "    let yyWildcard: CitronSymbolNumber? = nil\n\n");
  }

  // Rules

  fprintf(out, "    // Rules\n\n");

  fprintf(out, "    let yyRuleInfo: [(lhs: CitronSymbolNumber, nrhs: UInt)] = [\n");
  for(rp=lemp->rule; rp; rp=rp->next){
    fprintf(out, "        (lhs: %d, nrhs: %d),\n",rp->lhs->index,rp->nrhs); lineno++;
  }
  fprintf(out, "    ]\n\n");

  // Stack

  fprintf(out, "    // Stack\n\n");

  fprintf(out, "    var yyStack: [(stateOrRule: CitronStateOrRule , symbolCode: CitronSymbolNumber, symbol: CitronSymbol)]  = [\n");
  fprintf(out, "        (stateOrRule: .state(0), symbolCode: 0, symbol: .yyBaseOfStack)\n");
  fprintf(out, "    ]\n");
  fprintf(out, "    var maxStackSize: Int? = nil\n");
  fprintf(out, "    var maxAttainedStackSize: Int = 0\n\n");

  // Tracing

  fprintf(out, "    // Tracing\n\n");

  fprintf(out, "    var isTracingEnabled: Bool = false\n");
  fprintf(out, "    var isTracingPrintsSymbolValues: Bool = false\n");
  fprintf(out, "    var isTracingPrintsTokenValues: Bool = false\n");

  /* Generate a table containing the symbolic name of every symbol */
  fprintf(out, "    let yySymbolName: [String] = [\n");
  for(i=0; i<lemp->nsymbol; i++){
    fprintf(out, "    /* %2d */ \"%s\",\n", i, lemp->symbols[i]->name);
  }
  fprintf(out, "    ]\n");

  /* Generate a table containing a text string that describes every
  ** rule in the rule set of the grammar.  This information is used
  ** when tracing REDUCE actions.  */
  fprintf(out, "    let yyRuleText: [String] = [\n");
  for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
    assert( rp->iRule==i );
    fprintf(out,"        /* %3d */ \"", i);
    writeRuleText(out, rp);
    fprintf(out,"\",\n");
  }
  fprintf(out, "    ]\n\n");

  // Extra class members

  if (lemp->extraClassMembers) {
    fprintf(out, "    // Extra class members\n\n");
    fprintf(out, "%s\n\n", lemp->extraClassMembers);
  }

  // Function definitions

  fprintf(out, "    // Function definitions\n\n");

  fprintf(out, "    func yyTokenToSymbol(_ token: CitronToken) -> CitronSymbol {\n");
  fprintf(out, "        return .yy0(token)\n");
  fprintf(out, "    }\n\n");


  /* Generate code which execution during each REDUCE action */
  fprintf(out, "    func yyInvokeCodeBlockForRule(ruleNumber: CitronRuleNumber) throws -> CitronSymbol {\n");
  if (lemp->vartype && lemp->defaultCodeBlock) {
    fprintf(out, "        func defaultCodeBlockForDefaultNonterminalType() -> %s {", lemp->vartype);
    fprintf(out, "\n#sourceLocation(file: \"%s\", line: %d)\n", lemp->filename, lemp->defaultCodeBlockLineNumber);
    fprintf(out, "%s", lemp->defaultCodeBlock);
    fprintf(out, "\n#sourceLocation()\n");
    fprintf(out, "        }\n");
  }
  for (i = 1 /* Skip the base symbol */; i < lemp->nsymbol; i++) {
    struct symbol *sp = lemp->symbols[i];
    if (sp->type==NONTERMINAL && sp->datatype != 0 && sp->code != 0) {
      fprintf(out, "        func defaultCodeBlockForType%d() -> %s {", sp->dtnum, sp->datatype);
      fprintf(out, "\n#sourceLocation(file: \"%s\", line: %d)\n", lemp->filename, sp->codeLineNumber);
      fprintf(out, "%s", sp->code);
      fprintf(out, "\n#sourceLocation()\n");
      fprintf(out, "        }\n");
    }
  }

  fprintf(out, "        switch (ruleNumber) {\n");
  int ruleNumberMaxDigits = 0;
  i = lemp->nrule;
  while (i > 0) {
    i = i / 10;
    ruleNumberMaxDigits++;
  }
  i = 0;
  for(rp=lemp->rule; rp; rp=rp->next){
    int ruleNumber = rp->iRule;
    fprintf(out, "        case %d: /* ", rp->iRule);
    writeRuleText(out, rp);
    fprintf(out, " */\n");
    fprintf(out, "            func codeBlockForRule%0*d(",
            ruleNumberMaxDigits, rp->iRule);
    int is_first_rhs_item = 1;
    for (i = 0; i < rp->nrhs; i++) {
      const char *rhsalias = rp->rhsalias[i];
      if (rhsalias) {
        const char *rhstype = type_string_of_symbol(rp->rhs[i], lemp);
        assert(rhstype);
        fprintf(out, "%s%s: %s",
                (is_first_rhs_item? "" : ", "),
                rhsalias, rhstype);
        is_first_rhs_item = 0;
      }
    }
    assert(rp->lhs);
    const char *lhstype = type_string_of_symbol(rp->lhs, lemp);
    assert(lhstype);
    fprintf(out, ") throws -> %s {", lhstype);
    if (rp->code) {
      fprintf(out, "\n#sourceLocation(file: \"%s\", line: %d)\n", lemp->filename, rp->line);
      fprintf(out, "%s", rp->code);
      fprintf(out, "\n#sourceLocation()\n");
    } else if (rp->lhs->datatype && rp->lhs->code) {
      fprintf(out, "\n    return defaultCodeBlockForType%d()\n", rp->lhs->dtnum);
    } else if (rp->lhs->datatype == 0 && lemp->defaultCodeBlock) {
      fprintf(out, "\n    return defaultCodeBlockForDefaultNonterminalType()\n");
    }
    fprintf(out, "}\n");
    is_first_rhs_item = 1;
    for (i = 0; i < rp->nrhs; i++) {
      const char *rhsalias = rp->rhsalias[i];
      if (rhsalias) {
        int rhsdtnum = dtnum_of_symbol(rp->rhs[i]);
        assert(rhsdtnum >= 0);
        fprintf(out, "%s case .yy%d(let %s) = yySymbolOnStack(distanceFromTop: %d)",
                (is_first_rhs_item? "            if" : ",\n              "),
                rhsdtnum, rhsalias, rp->nrhs - 1 - i);
        is_first_rhs_item = 0;
      }
    }
    int has_aliased_rhs_items = (is_first_rhs_item == 0);
    if (has_aliased_rhs_items) {
        fprintf(out, " {\n    "); // Open if case
    }
    fprintf(out, "            return .yy%d(try codeBlockForRule%0*d(",
            rp->lhs->dtnum, ruleNumberMaxDigits, rp->iRule);
    is_first_rhs_item = 1;
    for (i = 0; i < rp->nrhs; i++) {
      const char *rhsalias = rp->rhsalias[i];
      if (rhsalias) {
        fprintf(out, "%s%s: %s", (is_first_rhs_item? "" : ", "), rhsalias, rhsalias);
        is_first_rhs_item = 0;
      }
    }
    fprintf(out, "))\n");
    if (has_aliased_rhs_items) {
        fprintf(out, "            }\n"); // Close if case
    }
  }
  fprintf(out, "        default:\n");
  fprintf(out, "            fatalError(\"Can't invoke code block for rule number \\(ruleNumber) - no such rule\")\n");
  fprintf(out, "        }\n"); // Close switch(ruleNumber)
  fprintf(out, "        fatalError(\"Can't resolve types correctly for invoking code block for rule number \\(ruleNumber)\")\n");
  fprintf(out, "    }\n\n"); // Close func invokeCodeBlockForRule(ruleNumber:)

  fprintf(out, "    private func yySymbolOnStack(distanceFromTop: Int) -> CitronSymbol {\n");
  fprintf(out, "        assert(yyStack.count > distanceFromTop)\n");
  fprintf(out, "        return yyStack[yyStack.count - 1 - distanceFromTop].symbol\n");
  fprintf(out, "    }\n\n");

  fprintf(out, "    func yyUnwrapResultFromSymbol(_ symbol: CitronSymbol) -> CitronResult {\n");
  fprintf(out, "        if case .yy%d(let result) = symbol {\n", dtnum_of_symbol(start_symbol));
  fprintf(out, "            return result\n");
  fprintf(out, "        } else {\n");
  fprintf(out, "            fatalError(\"Unexpected mismatch in result type\")\n");
  fprintf(out, "        }\n");
  fprintf(out, "    }\n\n");

  // Error capturing

  fprintf(out, "    // Error capturing\n\n");

  fprintf(out, "    typealias CitronErrorCaptureDelegate = _%sCitronErrorCaptureDelegate\n\n", className);
  fprintf(out, "    weak var errorCaptureDelegate: CitronErrorCaptureDelegate? = nil\n\n");
  int num_of_states_with_error_capturing = 0;
  if (lemp->has_error_capture) {
    fprintf(out, "    let yyErrorCaptureSymbolNumbersForState: [CitronStateNumber:[CitronSymbolNumber]] = [");
    for(i=0; i<lemp->nxstate; i++) {
      struct state *stp = lemp->sorted[i];
      int num_of_error_capturing_sequences = 0;
      struct symbol *last_seen_symbol = 0;
      for (struct config *cfg = stp->cfp; cfg != 0; cfg = cfg->next) {
        // We need only configs that begin with the dot
        if (cfg->dot > 0) { continue; }
        struct symbol *lhs_symbol = cfg->rp->lhs;
        if (lhs_symbol == last_seen_symbol) {
          continue;
        }
        last_seen_symbol = lhs_symbol;
        // We need only symbols that have %capture_errors defined on them
        if (lhs_symbol->error_capture_line == 0) {
          continue;
        }
        // We consider only symbols that can be shifted in from this state
        int is_symbol_shiftable = 0;
        for (struct action *ap = stp->ap; ap != 0; ap = ap->next) {
          if (ap->sp == lhs_symbol && (ap->type == SHIFT || ap->type == ACCEPT || ap->type == SHIFTREDUCE)) {
            is_symbol_shiftable = 1;
            break;
          }
        }
        if (!is_symbol_shiftable) {
          continue;
        }
        // We now have a valid symbol we can write out
        if (num_of_error_capturing_sequences == 0) {
            fprintf(out, "\n        %3d : [", i);
        } else {
          fprintf(out, ", ");
        }
        fprintf(out, "%d", lhs_symbol->index);
        num_of_error_capturing_sequences++;
      }
      if (num_of_error_capturing_sequences > 0) {
        fprintf(out, "],");
        num_of_states_with_error_capturing++;
      }
    }
    if (num_of_states_with_error_capturing > 0) {
      fprintf(out, "\n    ]\n");
    } else {
      fprintf(out, ":]\n");
    }
  }
  if (num_of_states_with_error_capturing > 0) {
    fprintf(out, "    let yyCanErrorCapture: Bool = true\n");

    int *is_error_capture_end_before_token = (int *) calloc(lemp->nsymbol, sizeof(int));
    int *is_error_capture_end_after_sequence_last_token = (int *) calloc(lemp->nsymbol, sizeof(int));
    for(int i=0; i<lemp->nsymbol; i++){
      is_error_capture_end_before_token[i] = 0;
      is_error_capture_end_after_sequence_last_token[i] = 0;
    }

    fprintf(out, "    let yyErrorCaptureDirectives: [CitronSymbolNumber:(endAfter:[[CitronTokenCode]],endBefore:[CitronTokenCode])] = [\n");
    int is_first_error_capture_symbol = 1;
    for(int i=0; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if (sp->num_error_capture_end_before_sequences == 0 &&
          sp->num_error_capture_end_after_sequences == 0) {
        continue;
      }
      if (is_first_error_capture_symbol == 0) { fprintf(out, ",\n"); }
      is_first_error_capture_symbol = 0;
      struct token_sequence *end_sequence_lists[] = {
        sp->error_capture_end_before_sequences,
        sp->error_capture_end_after_sequences,
      };
      int end_sequence_list_lengths[] = {
        sp->num_error_capture_end_before_sequences,
        sp->num_error_capture_end_after_sequences,
      };
      const char *tokenPrefix = lemp->tokenprefix ? lemp->tokenprefix : "";
      fprintf(out, "       %3d : (endAfter: [", sp->index);
      for (int i = 0; i < sp->num_error_capture_end_after_sequences; i++) {
        struct token_sequence seq = sp->error_capture_end_after_sequences[i];
        if (i > 0) { fprintf(out, ", "); }
        if (seq.count == 1) {
          struct symbol *ts = seq.tokens.single_token;
          fprintf(out, "[.%s%s]", tokenPrefix, ts->name);
          is_error_capture_end_after_sequence_last_token[ts->index] = 1;
        } else if (seq.count > 1) {
          fprintf(out, "[");
          for (int j = 0; j < seq.count; j++) {
            if (j > 0) { fprintf(out, ", "); }
            struct symbol *ts = seq.tokens.multiple_tokens[j];
            fprintf(out, ".%s%s", tokenPrefix, ts->name);
            if (j == seq.count - 1) { // If this is the last token in the sequence
              is_error_capture_end_after_sequence_last_token[ts->index] = 1;
            }
          }
          fprintf(out, "]");
        }
      }
      fprintf(out, "],\n");
      fprintf(out, "              endBefore: [");
      for (int i = 0; i < sp->num_error_capture_end_before_sequences; i++) {
        struct token_sequence seq = sp->error_capture_end_before_sequences[i];
        if (i > 0) { fprintf(out, ", "); }
        assert(seq.count == 1); /* token sequences are not allowed in end_before clauses */
        if (seq.count == 1) {
          struct symbol *ts = seq.tokens.single_token;
          fprintf(out, ".%s%s", tokenPrefix, ts->name);
          is_error_capture_end_before_token[ts->index] = 1;
        }
      }
      fprintf(out, "])");
    }
    fprintf(out, "\n    ]\n");

    fprintf(out, "    let yyErrorCaptureEndBeforeTokens: Set<CitronSymbolNumber> = [\n");
    fprintf(out, "        ");
    int is_first_error_capture_end_before_token = 1;
    for(int i=0; i<lemp->nsymbol; i++){
      if (is_error_capture_end_before_token[i]) {
        if (is_first_error_capture_end_before_token == 0) { fprintf(out, ", "); }
        is_first_error_capture_end_before_token = 0;
        fprintf(out, "%d", lemp->symbols[i]->index);
      }
    }
    fprintf(out, "\n    ]\n");
    fprintf(out, "    let yyErrorCaptureEndAfterSequenceEndingTokens: Set<CitronSymbolNumber> = [\n");
    fprintf(out, "        ");
    int is_first_error_capture_end_after_sequence_last_token = 1;
    for(int i=0; i<lemp->nsymbol; i++){
      if (is_error_capture_end_after_sequence_last_token[i]) {
        if (is_first_error_capture_end_after_sequence_last_token == 0) { fprintf(out, ", "); }
        is_first_error_capture_end_after_sequence_last_token = 0;
        fprintf(out, "%d", lemp->symbols[i]->index);
      }
    }
    fprintf(out, "\n    ]\n\n");

    fprintf(out, "    func yyShouldSaveErrorForCapturing(error: Error) -> Bool {\n");
    fprintf(out, "        guard let delegate = errorCaptureDelegate else {\n");
    fprintf(out, "            print(\"Error capture: Not saving error for capturing because errorCaptureDelegate is not set\")\n");
    fprintf(out, "            return false\n");
    fprintf(out, "        }\n");
    fprintf(out, "        return delegate.shouldSaveErrorForCapturing(error: error)\n");
    fprintf(out, "    }\n\n");

    fprintf(out, "    func yyCaptureError(on symbolCode: CitronNonTerminalCode, error: Error, state: CitronErrorCaptureState) -> CitronSymbol? {\n");
    fprintf(out, "        guard let delegate = errorCaptureDelegate else {\n");
    fprintf(out, "            print(\"Error capture: Not capturing error because errorCaptureDelegate is not set\")\n");
    fprintf(out, "            return nil\n");
    fprintf(out, "        }\n");
    fprintf(out, "\n");
    fprintf(out, "        switch (symbolCode) {\n");

    int numberOfSymbolsInyyCaptureError = 0;
    for(int i=lemp->nterminal; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if (sp->error_capture_line == 0) {
        continue;
      }
      numberOfSymbolsInyyCaptureError++;
      assert(sp->type == NONTERMINAL);
      fprintf(out, "        case .%s%s:\n", nonterminalPrefix, sp->name);
      fprintf(out, "            let delegateResponse = delegate.shouldCaptureErrorOn%c%s(state: state, error: error)\n", TOUPPER(sp->name[0]), sp->name + 1);
      fprintf(out, "            switch (delegateResponse) {\n");
      fprintf(out, "            case .captureAs(let symbol):\n");
      fprintf(out, "                return .yy%d(symbol)\n", sp->dtnum);
      fprintf(out, "            case .dontCapture:\n");
      fprintf(out, "                return nil\n");
      fprintf(out, "            }\n");
    }
    if (numberOfSymbolsInyyCaptureError < (lemp->nsymbol - lemp->nterminal)) {
      fprintf(out, "        default:\n");
      fprintf(out, "            fatalError(\"yyCaptureError: Symbol code \\(symbolCode) is not an error capturing symbol code\")\n");
    }
    fprintf(out, "        }\n");
    fprintf(out, "    }\n\n");
  } else {
    fprintf(out, "    let yyErrorCaptureSymbolNumbersForState: [CitronStateNumber:[CitronSymbolNumber]] = [:]\n");
    fprintf(out, "    let yyCanErrorCapture: Bool = false\n");
    fprintf(out, "    let yyErrorCaptureDirectives: [CitronSymbolNumber:(endAfter:[[CitronTokenCode]],endBefore:[CitronTokenCode])] = [:]\n");
    fprintf(out, "    let yyErrorCaptureEndBeforeTokens: Set<CitronSymbolNumber> = []\n\n");
    fprintf(out, "    let yyErrorCaptureEndAfterSequenceEndingTokens: Set<CitronSymbolNumber> = []\n\n");

    fprintf(out, "    func yyShouldSaveErrorForCapturing(error: Error) -> Bool {\n");
    fprintf(out, "        fatalError(\"This parser was not generated with error capturing information\")\n");
    fprintf(out, "    }\n\n");

    fprintf(out, "    func yyCaptureError(on symbolCode: CitronNonTerminalCode, error: Error, state: CitronErrorCaptureState) -> CitronSymbol? {\n");
    fprintf(out, "        fatalError(\"This parser was not generated with error capturing information\")\n");
    fprintf(out, "    }\n\n");
  }

  // Find the end state
  int end_state_index = -1;
  for(i=0; i<lemp->nxstate; i++) {
    struct state *stp = lemp->sorted[i];
    for (struct config *cfg = stp->cfp; cfg != 0; cfg = cfg->next) {
      if (cfg->rp->lhs == start_symbol) {
        if (cfg->dot == cfg->rp->nrhs) {
          end_state_index = i;
          break;
        }
      }
    }
    if (end_state_index >= 0) {
      break;
    }
  }
  assert(end_state_index >= 0);

  fprintf(out, "    func yySymbolContent(_ symbol: CitronSymbol) -> Any { return symbol.typeErasedContent() }\n\n");
  fprintf(out, "    let yyStartSymbolNumber: CitronSymbolNumber = %d\n", start_symbol->index);
  fprintf(out, "    let yyEndStateNumber: CitronStateNumber = %d\n\n", end_state_index);
  fprintf(out, "    var yyErrorCaptureSavedError: (error: Error, isLexerError: Bool)? = nil\n");
  fprintf(out, "    var yyErrorCaptureTokensSinceError: [(token: CitronToken, tokenCode: CitronTokenCode)] = []\n");
  fprintf(out, "    var yyErrorCaptureStackIndices: [Int] = []\n");
  fprintf(out, "    var yyErrorCaptureStartSymbolStackIndex: Int? = nil\n\n");
  fprintf(out, "    var numberOfCapturedErrors: Int = 0\n");

  fprintf(out, "}\n\n"); // Closing class Parser

  fprintf(out, "protocol _%sCitronErrorCaptureDelegate : AnyObject {\n", className);
  fprintf(out, "    func shouldSaveErrorForCapturing(error: Error) -> Bool\n");
  for(int i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    if (sp->error_capture_line == 0) {
      continue;
    }
    fprintf(out, "\n    /* %s */\n", sp->name);
    fprintf(out, "    func shouldCaptureErrorOn%c%s(", TOUPPER(sp->name[0]), sp->name + 1);
    fprintf(out, "state: %s.CitronErrorCaptureState,\n", className);
    fprintf(out, "        error: Error)");
    fprintf(out, " -> CitronErrorCaptureResponse<%s>\n", type_string_of_symbol(sp, lemp));
  }
  fprintf(out, "}\n\n");
  fprintf(out, "extension _%sCitronErrorCaptureDelegate {\n", className);
  fprintf(out, "    func shouldSaveErrorForCapturing(error: Error) -> Bool {\n");
  fprintf(out, "        return true\n");
  fprintf(out, "    }\n");
  fprintf(out, "}\n\n");

  fprintf(out, "// Ability to use == to compare CitronSymbolCode with CitronTokenCode / CitronNonTerminalCode\n\n");
  fprintf(out, "extension %s.CitronSymbolCode {\n", className);
  fprintf(out, "    static func == (a: %s.CitronSymbolCode, b: %s.CitronTokenCode) -> Bool {\n", className, className);
  fprintf(out, "        guard case let .token(code) = a else { return false }\n");
  fprintf(out, "        return (code == b)\n");
  fprintf(out, "    }\n");
  fprintf(out, "    static func == (a: %s.CitronTokenCode, b: %s.CitronSymbolCode) -> Bool {\n", className, className);
  fprintf(out, "        guard case let .token(code) = b else { return false }\n");
  fprintf(out, "        return (code == a)\n");
  fprintf(out, "    }\n");
  fprintf(out, "    static func == (a: %s.CitronSymbolCode, b: %s.CitronNonTerminalCode) -> Bool {\n", className, className);
  fprintf(out, "        guard case let .nonterminal(code) = a else { return false }\n");
  fprintf(out, "        return (code == b)\n");
  fprintf(out, "    }\n");
  fprintf(out, "    static func == (a: %s.CitronNonTerminalCode, b: %s.CitronSymbolCode) -> Bool {\n", className, className);
  fprintf(out, "        guard case let .nonterminal(code) = b else { return false }\n");
  fprintf(out, "        return (code == a)\n");
  fprintf(out, "    }\n");
  fprintf(out, "}\n\n");

  fprintf(out, "// Ability to use switch (symbolCode) { case .tokenCode: ...; case .nonterminalCode: ... }\n\n");
  fprintf(out, "extension %s.CitronSymbolCode {\n", className);
  fprintf(out, "    static func ~= (pattern: %s.CitronTokenCode, value: %s.CitronSymbolCode) -> Bool {\n", className, className);
  fprintf(out, "        return (pattern == value)\n");
  fprintf(out, "    }\n");
  fprintf(out, "    static func ~= (pattern: %s.CitronNonTerminalCode, value: %s.CitronSymbolCode) -> Bool {\n", className, className);
  fprintf(out, "        return (pattern == value)\n");
  fprintf(out, "    }\n");
  fprintf(out, "}\n");

  // Epilogue

  if (lemp->epilogue) {
    fprintf(out, "// Epilogue\n\n");
    fprintf(out, "#sourceLocation(file: \"%s\", line: %d)\n", lemp->filename, lemp->epilogueLineNumber);
    fprintf(out, "%s\n", lemp->epilogue);
    fprintf(out, "#sourceLocation()\n\n");
  }

  fclose(out);
  return;
}

/* Reduce the size of the action tables, if possible, by making use
** of defaults.
**
** In this version, we take the most frequent REDUCE action and make
** it the default.  Except, there is no default if the wildcard token
** is a possible look-ahead.
*/
void CompressTables(struct lemon *lemp)
{
  struct state *stp;
  struct action *ap, *ap2, *nextap;
  struct rule *rp, *rp2, *rbest;
  int nbest, n;
  int i;
  int usesWildcard;

  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    nbest = 0;
    rbest = 0;
    usesWildcard = 0;

    for(ap=stp->ap; ap; ap=ap->next){
      if( ap->type==SHIFT && ap->sp==lemp->wildcard ){
        usesWildcard = 1;
      }
      if( ap->type!=REDUCE ) continue;
      rp = ap->x.rp;
      if( rp->lhsStart ) continue;
      if( rp==rbest ) continue;
      n = 1;
      for(ap2=ap->next; ap2; ap2=ap2->next){
        if( ap2->type!=REDUCE ) continue;
        rp2 = ap2->x.rp;
        if( rp2==rbest ) continue;
        if( rp2==rp ) n++;
      }
      if( n>nbest ){
        nbest = n;
        rbest = rp;
      }
    }

    /* Do not make a default if the number of rules to default
    ** is not at least 1 or if the wildcard token is a possible
    ** lookahead.
    */
    if( nbest<1 || usesWildcard ) continue;


    /* Combine matching REDUCE actions into a single default */
    for(ap=stp->ap; ap; ap=ap->next){
      if( ap->type==REDUCE && ap->x.rp==rbest ) break;
    }
    assert( ap );
    ap->sp = Symbol_new("{default}");
    for(ap=ap->next; ap; ap=ap->next){
      if( ap->type==REDUCE && ap->x.rp==rbest ) ap->type = NOT_USED;
    }
    stp->ap = Action_sort(stp->ap);

    for(ap=stp->ap; ap; ap=ap->next){
      if( ap->type==SHIFT ) break;
      if( ap->type==REDUCE && ap->x.rp!=rbest ) break;
    }
    if( ap==0 ){
      stp->autoReduce = 1;
      stp->pDfltReduce = rbest;
    }
  }

  /* Make a second pass over all states and actions.  Convert
  ** every action that is a SHIFT to an autoReduce state into
  ** a SHIFTREDUCE action.
  */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(ap=stp->ap; ap; ap=ap->next){
      struct state *pNextState;
      if( ap->type!=SHIFT ) continue;
      pNextState = ap->x.stp;
      if( pNextState->autoReduce && pNextState->pDfltReduce!=0 ){
        ap->type = SHIFTREDUCE;
        ap->x.rp = pNextState->pDfltReduce;
      }
    }
  }

  /* If a SHIFTREDUCE action specifies a rule that has a single RHS term
  ** (meaning that the SHIFTREDUCE will land back in the state where it
  ** started) and if there is no C-code associated with the reduce action,
  ** then we can go ahead and convert the action to be the same as the
  ** action for the RHS of the rule.
  */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(ap=stp->ap; ap; ap=nextap){
      nextap = ap->next;
      if( ap->type!=SHIFTREDUCE ) continue;
      rp = ap->x.rp;
      if( rp->noCode==0 ) continue;
      if( rp->nrhs!=1 ) continue;
#if 1
      /* Only apply this optimization to non-terminals.  It would be OK to
      ** apply it to terminal symbols too, but that makes the parser tables
      ** larger. */
      if( ap->sp->index<lemp->nterminal ) continue;
#endif
      /* If we reach this point, it means the optimization can be applied */
      nextap = ap;
      for(ap2=stp->ap; ap2 && (ap2==ap || ap2->sp!=rp->lhs); ap2=ap2->next){}
      assert( ap2!=0 );
      ap->spOpt = ap2->sp;
      ap->type = ap2->type;
      ap->x = ap2->x;
    }
  }
}


/*
** Compare two states for sorting purposes.  The smaller state is the
** one with the most non-terminal actions.  If they have the same number
** of non-terminal actions, then the smaller is the one with the most
** token actions.
*/
static int stateResortCompare(const void *a, const void *b){
  const struct state *pA = *(const struct state**)a;
  const struct state *pB = *(const struct state**)b;
  int n;

  n = pB->nNtAct - pA->nNtAct;
  if( n==0 ){
    n = pB->nTknAct - pA->nTknAct;
    if( n==0 ){
      n = pB->statenum - pA->statenum;
    }
  }
  assert( n!=0 );
  return n;
}


/*
** Renumber and resort states so that states with fewer choices
** occur at the end.  Except, keep state 0 as the first state.
*/
void ResortStates(struct lemon *lemp)
{
  int i;
  struct state *stp;
  struct action *ap;

  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    stp->nTknAct = stp->nNtAct = 0;
    stp->iDfltReduce = lemp->nrule;  /* Init dflt action to "syntax error" */
    stp->iTknOfst = NO_OFFSET;
    stp->iNtOfst = NO_OFFSET;
    for(ap=stp->ap; ap; ap=ap->next){
      int iAction = compute_action(lemp,ap);
      if( iAction>=0 ){
        if( ap->sp->index<lemp->nterminal ){
          stp->nTknAct++;
        }else if( ap->sp->index<lemp->nsymbol ){
          stp->nNtAct++;
        }else{
          assert( stp->autoReduce==0 || stp->pDfltReduce==ap->x.rp );
          stp->iDfltReduce = iAction - lemp->nstate - lemp->nrule;
        }
      }
    }
  }
  qsort(&lemp->sorted[1], lemp->nstate-1, sizeof(lemp->sorted[0]),
        stateResortCompare);
  for(i=0; i<lemp->nstate; i++){
    lemp->sorted[i]->statenum = i;
  }
  lemp->nxstate = lemp->nstate;
  while( lemp->nxstate>1 && lemp->sorted[lemp->nxstate-1]->autoReduce ){
    lemp->nxstate--;
  }
}


/***************** From the file "set.c" ************************************/
/*
** Set manipulation routines for the LEMON parser generator.
*/

static int size = 0;

/* Set the set size */
void SetSize(int n)
{
  size = n+1;
}

/* Allocate a new set */
char *SetNew(void){
  char *s;
  s = (char*)calloc( size, 1);
  if( s==0 ){
    extern void memory_error();
    memory_error();
  }
  return s;
}

/* Deallocate a set */
void SetFree(char *s)
{
  free(s);
}

/* Add a new element to the set.  Return TRUE if the element was added
** and FALSE if it was already there. */
int SetAdd(char *s, int e)
{
  int rv;
  assert( e>=0 && e<size );
  rv = s[e];
  s[e] = 1;
  return !rv;
}

/* Add every element of s2 to s1.  Return TRUE if s1 changes. */
int SetUnion(char *s1, char *s2)
{
  int i, progress;
  progress = 0;
  for(i=0; i<size; i++){
    if( s2[i]==0 ) continue;
    if( s1[i]==0 ){
      progress = 1;
      s1[i] = 1;
    }
  }
  return progress;
}
/********************** From the file "table.c" ****************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/

PRIVATE unsigned strhash(const char *x)
{
  unsigned h = 0;
  while( *x ) h = h*13 + *(x++);
  return h;
}

/* Works like strdup, sort of.  Save a string in malloced memory, but
** keep strings in a table so that the same string is not in more
** than one place.
*/
const char *Strsafe(const char *y)
{
  const char *z;
  char *cpy;

  if( y==0 ) return 0;
  z = Strsafe_find(y);
  if( z==0 && (cpy=(char *)malloc( lemonStrlen(y)+1 ))!=0 ){
    lemon_strcpy(cpy,y);
    z = cpy;
    Strsafe_insert(z);
  }
  MemoryCheck(z);
  return z;
}

/* There is one instance of the following structure for each
** associative array of type "x1".
*/
struct s_x1 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x1node *tbl;  /* The data stored here */
  struct s_x1node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x1".
*/
typedef struct s_x1node {
  const char *data;        /* The data */
  struct s_x1node *next;   /* Next entry with the same hash */
  struct s_x1node **from;  /* Previous link */
} x1node;

/* There is only one instance of the array, which is the following */
static struct s_x1 *x1a;

/* Allocate a new associative array */
void Strsafe_init(void){
  if( x1a ) return;
  x1a = (struct s_x1*)malloc( sizeof(struct s_x1) );
  if( x1a ){
    x1a->size = 1024;
    x1a->count = 0;
    x1a->tbl = (x1node*)calloc(1024, sizeof(x1node) + sizeof(x1node*));
    if( x1a->tbl==0 ){
      free(x1a);
      x1a = 0;
    }else{
      int i;
      x1a->ht = (x1node**)&(x1a->tbl[1024]);
      for(i=0; i<1024; i++) x1a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Strsafe_insert(const char *data)
{
  x1node *np;
  unsigned h;
  unsigned ph;

  if( x1a==0 ) return 0;
  ph = strhash(data);
  h = ph & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x1a->count>=x1a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x1 array;
    array.size = arrSize = x1a->size*2;
    array.count = x1a->count;
    array.tbl = (x1node*)calloc(arrSize, sizeof(x1node) + sizeof(x1node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x1node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x1a->count; i++){
      x1node *oldnp, *newnp;
      oldnp = &(x1a->tbl[i]);
      h = strhash(oldnp->data) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x1a->tbl);
    *x1a = array;
  }
  /* Insert the new data */
  h = ph & (x1a->size-1);
  np = &(x1a->tbl[x1a->count++]);
  np->data = data;
  if( x1a->ht[h] ) x1a->ht[h]->from = &(np->next);
  np->next = x1a->ht[h];
  x1a->ht[h] = np;
  np->from = &(x1a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
const char *Strsafe_find(const char *key)
{
  unsigned h;
  x1node *np;

  if( x1a==0 ) return 0;
  h = strhash(key) & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return a pointer to the (terminal or nonterminal) symbol "x".
** Create a new symbol if this is the first time "x" has been seen.
*/
struct symbol *Symbol_new(const char *x)
{
  struct symbol *sp;

  sp = Symbol_find(x);
  if( sp==0 ){
    sp = (struct symbol *)calloc(1, sizeof(struct symbol) );
    MemoryCheck(sp);
    sp->name = Strsafe(x);
    sp->type = ISUPPER(*x) ? TERMINAL : NONTERMINAL;
    sp->rule = 0;
    sp->fallback = 0;
    sp->prec = -1;
    sp->assoc = UNK;
    sp->firstset = 0;
    sp->lambda = LEMON_FALSE;
    sp->destructor = 0;
    sp->destLineno = 0;
    sp->datatype = 0;
    sp->useCnt = 0;
    sp->code = 0;
    sp->codeLineNumber = 0;
    sp->error_capture_line = 0;
    sp->num_error_capture_end_before_sequences = 0;
    sp->num_error_capture_end_after_sequences = 0;
    sp->error_capture_end_before_sequences = 0;
    sp->error_capture_end_after_sequences = 0;
    Symbol_insert(sp,sp->name);
  }
  sp->useCnt++;
  return sp;
}

/* Compare two symbols for sorting purposes.  Return negative,
** zero, or positive if a is less then, equal to, or greater
** than b.
**
** Symbols that begin with upper case letters (terminals or tokens)
** must sort before symbols that begin with lower case letters
** (non-terminals).  And MULTITERMINAL symbols (created using the
** %token_class directive) must sort at the very end. Other than
** that, the order does not matter.
**
** We find experimentally that leaving the symbols in their original
** order (the order they appeared in the grammar file) gives the
** smallest parser tables in SQLite.
*/
int Symbolcmpp(const void *_a, const void *_b)
{
  const struct symbol *a = *(const struct symbol **) _a;
  const struct symbol *b = *(const struct symbol **) _b;
  int i1 = a->type==MULTITERMINAL ? 3 : a->name[0]>'Z' ? 2 : 1;
  int i2 = b->type==MULTITERMINAL ? 3 : b->name[0]>'Z' ? 2 : 1;
  return i1==i2 ? a->index - b->index : i1 - i2;
}

/* There is one instance of the following structure for each
** associative array of type "x2".
*/
struct s_x2 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x2node *tbl;  /* The data stored here */
  struct s_x2node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x2".
*/
typedef struct s_x2node {
  struct symbol *data;     /* The data */
  const char *key;         /* The key */
  struct s_x2node *next;   /* Next entry with the same hash */
  struct s_x2node **from;  /* Previous link */
} x2node;

/* There is only one instance of the array, which is the following */
static struct s_x2 *x2a;

/* Allocate a new associative array */
void Symbol_init(void){
  if( x2a ) return;
  x2a = (struct s_x2*)malloc( sizeof(struct s_x2) );
  if( x2a ){
    x2a->size = 128;
    x2a->count = 0;
    x2a->tbl = (x2node*)calloc(128, sizeof(x2node) + sizeof(x2node*));
    if( x2a->tbl==0 ){
      free(x2a);
      x2a = 0;
    }else{
      int i;
      x2a->ht = (x2node**)&(x2a->tbl[128]);
      for(i=0; i<128; i++) x2a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Symbol_insert(struct symbol *data, const char *key)
{
  x2node *np;
  unsigned h;
  unsigned ph;

  if( x2a==0 ) return 0;
  ph = strhash(key);
  h = ph & (x2a->size-1);
  np = x2a->ht[h];
  while( np ){
    if( strcmp(np->key,key)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x2a->count>=x2a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x2 array;
    array.size = arrSize = x2a->size*2;
    array.count = x2a->count;
    array.tbl = (x2node*)calloc(arrSize, sizeof(x2node) + sizeof(x2node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x2node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x2a->count; i++){
      x2node *oldnp, *newnp;
      oldnp = &(x2a->tbl[i]);
      h = strhash(oldnp->key) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->key = oldnp->key;
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x2a->tbl);
    *x2a = array;
  }
  /* Insert the new data */
  h = ph & (x2a->size-1);
  np = &(x2a->tbl[x2a->count++]);
  np->key = key;
  np->data = data;
  if( x2a->ht[h] ) x2a->ht[h]->from = &(np->next);
  np->next = x2a->ht[h];
  x2a->ht[h] = np;
  np->from = &(x2a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct symbol *Symbol_find(const char *key)
{
  unsigned h;
  x2node *np;

  if( x2a==0 ) return 0;
  h = strhash(key) & (x2a->size-1);
  np = x2a->ht[h];
  while( np ){
    if( strcmp(np->key,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return the n-th data.  Return NULL if n is out of range. */
struct symbol *Symbol_Nth(int n)
{
  struct symbol *data;
  if( x2a && n>0 && n<=x2a->count ){
    data = x2a->tbl[n-1].data;
  }else{
    data = 0;
  }
  return data;
}

/* Return the size of the array */
int Symbol_count()
{
  return x2a ? x2a->count : 0;
}

/* Return an array of pointers to all data in the table.
** The array is obtained from malloc.  Return NULL if memory allocation
** problems, or if the array is empty. */
struct symbol **Symbol_arrayof()
{
  struct symbol **array;
  int i,arrSize;
  if( x2a==0 ) return 0;
  arrSize = x2a->count;
  array = (struct symbol **)calloc(arrSize, sizeof(struct symbol *));
  if( array ){
    for(i=0; i<arrSize; i++) array[i] = x2a->tbl[i].data;
  }
  return array;
}

/* Compare two configurations */
int Configcmp(const char *_a,const char *_b)
{
  const struct config *a = (struct config *) _a;
  const struct config *b = (struct config *) _b;
  int x;
  x = a->rp->index - b->rp->index;
  if( x==0 ) x = a->dot - b->dot;
  return x;
}

/* Compare two states */
PRIVATE int statecmp(struct config *a, struct config *b)
{
  int rc;
  for(rc=0; rc==0 && a && b;  a=a->bp, b=b->bp){
    rc = a->rp->index - b->rp->index;
    if( rc==0 ) rc = a->dot - b->dot;
  }
  if( rc==0 ){
    if( a ) rc = 1;
    if( b ) rc = -1;
  }
  return rc;
}

/* Hash a state */
PRIVATE unsigned statehash(struct config *a)
{
  unsigned h=0;
  while( a ){
    h = h*571 + a->rp->index*37 + a->dot;
    a = a->bp;
  }
  return h;
}

/* Allocate a new state structure */
struct state *State_new()
{
  struct state *newstate;
  newstate = (struct state *)calloc(1, sizeof(struct state) );
  MemoryCheck(newstate);
  return newstate;
}

/* There is one instance of the following structure for each
** associative array of type "x3".
*/
struct s_x3 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x3node *tbl;  /* The data stored here */
  struct s_x3node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x3".
*/
typedef struct s_x3node {
  struct state *data;                  /* The data */
  struct config *key;                   /* The key */
  struct s_x3node *next;   /* Next entry with the same hash */
  struct s_x3node **from;  /* Previous link */
} x3node;

/* There is only one instance of the array, which is the following */
static struct s_x3 *x3a;

/* Allocate a new associative array */
void State_init(void){
  if( x3a ) return;
  x3a = (struct s_x3*)malloc( sizeof(struct s_x3) );
  if( x3a ){
    x3a->size = 128;
    x3a->count = 0;
    x3a->tbl = (x3node*)calloc(128, sizeof(x3node) + sizeof(x3node*));
    if( x3a->tbl==0 ){
      free(x3a);
      x3a = 0;
    }else{
      int i;
      x3a->ht = (x3node**)&(x3a->tbl[128]);
      for(i=0; i<128; i++) x3a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int State_insert(struct state *data, struct config *key)
{
  x3node *np;
  unsigned h;
  unsigned ph;

  if( x3a==0 ) return 0;
  ph = statehash(key);
  h = ph & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x3a->count>=x3a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x3 array;
    array.size = arrSize = x3a->size*2;
    array.count = x3a->count;
    array.tbl = (x3node*)calloc(arrSize, sizeof(x3node) + sizeof(x3node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x3node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x3a->count; i++){
      x3node *oldnp, *newnp;
      oldnp = &(x3a->tbl[i]);
      h = statehash(oldnp->key) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->key = oldnp->key;
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x3a->tbl);
    *x3a = array;
  }
  /* Insert the new data */
  h = ph & (x3a->size-1);
  np = &(x3a->tbl[x3a->count++]);
  np->key = key;
  np->data = data;
  if( x3a->ht[h] ) x3a->ht[h]->from = &(np->next);
  np->next = x3a->ht[h];
  x3a->ht[h] = np;
  np->from = &(x3a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct state *State_find(struct config *key)
{
  unsigned h;
  x3node *np;

  if( x3a==0 ) return 0;
  h = statehash(key) & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return an array of pointers to all data in the table.
** The array is obtained from malloc.  Return NULL if memory allocation
** problems, or if the array is empty. */
struct state **State_arrayof(void)
{
  struct state **array;
  int i,arrSize;
  if( x3a==0 ) return 0;
  arrSize = x3a->count;
  array = (struct state **)calloc(arrSize, sizeof(struct state *));
  if( array ){
    for(i=0; i<arrSize; i++) array[i] = x3a->tbl[i].data;
  }
  return array;
}

/* Hash a configuration */
PRIVATE unsigned confighash(struct config *a)
{
  unsigned h=0;
  h = h*571 + a->rp->index*37 + a->dot;
  return h;
}

/* There is one instance of the following structure for each
** associative array of type "x4".
*/
struct s_x4 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x4node *tbl;  /* The data stored here */
  struct s_x4node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x4".
*/
typedef struct s_x4node {
  struct config *data;                  /* The data */
  struct s_x4node *next;   /* Next entry with the same hash */
  struct s_x4node **from;  /* Previous link */
} x4node;

/* There is only one instance of the array, which is the following */
static struct s_x4 *x4a;

/* Allocate a new associative array */
void Configtable_init(void){
  if( x4a ) return;
  x4a = (struct s_x4*)malloc( sizeof(struct s_x4) );
  if( x4a ){
    x4a->size = 64;
    x4a->count = 0;
    x4a->tbl = (x4node*)calloc(64, sizeof(x4node) + sizeof(x4node*));
    if( x4a->tbl==0 ){
      free(x4a);
      x4a = 0;
    }else{
      int i;
      x4a->ht = (x4node**)&(x4a->tbl[64]);
      for(i=0; i<64; i++) x4a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Configtable_insert(struct config *data)
{
  x4node *np;
  unsigned h;
  unsigned ph;

  if( x4a==0 ) return 0;
  ph = confighash(data);
  h = ph & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp((const char *) np->data,(const char *) data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x4a->count>=x4a->size ){
    /* Need to make the hash table bigger */
    int i,arrSize;
    struct s_x4 array;
    array.size = arrSize = x4a->size*2;
    array.count = x4a->count;
    array.tbl = (x4node*)calloc(arrSize, sizeof(x4node) + sizeof(x4node*));
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x4node**)&(array.tbl[arrSize]);
    for(i=0; i<arrSize; i++) array.ht[i] = 0;
    for(i=0; i<x4a->count; i++){
      x4node *oldnp, *newnp;
      oldnp = &(x4a->tbl[i]);
      h = confighash(oldnp->data) & (arrSize-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x4a->tbl);
    *x4a = array;
  }
  /* Insert the new data */
  h = ph & (x4a->size-1);
  np = &(x4a->tbl[x4a->count++]);
  np->data = data;
  if( x4a->ht[h] ) x4a->ht[h]->from = &(np->next);
  np->next = x4a->ht[h];
  x4a->ht[h] = np;
  np->from = &(x4a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct config *Configtable_find(struct config *key)
{
  int h;
  x4node *np;

  if( x4a==0 ) return 0;
  h = confighash(key) & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp((const char *) np->data,(const char *) key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Remove all data from the table.  Pass each data to the function "f"
** as it is removed.  ("f" may be null to avoid this step.) */
void Configtable_clear(int(*f)(struct config *))
{
  int i;
  if( x4a==0 || x4a->count==0 ) return;
  if( f ) for(i=0; i<x4a->count; i++) (*f)(x4a->tbl[i].data);
  for(i=0; i<x4a->size; i++) x4a->ht[i] = 0;
  x4a->count = 0;
  return;
}
