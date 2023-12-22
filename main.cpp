#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <iostream>
using namespace std;

/*
{ Sample program
  in TINY language
  compute factorial
}

read x; {input an integer}
if 0<x then {compute only if x>=1}
  fact:=1;
  repeat
    fact := fact * x;
    x:=x-1
  until x=0;
  write fact {output factorial}
end
*/

// sequence of statements separated by ;
// no procedures - no declarations
// all variables are integers
// variables are declared simply by assigning values to them :=
// if-statement: if (boolean) then [else] end
// repeat-statement: repeat until (boolean)
// boolean only in if and repeat conditions < = and two mathematical expressions
// math expressions integers only, + - * / ^
// I/O read write
// Comments {}

////////////////////////////////////////////////////////////////////////////////////
// Strings /////////////////////////////////////////////////////////////////////////

bool Equals(const char* a, const char* b)
{
    return strcmp(a, b)==0;
}

bool StartsWith(const char* a, const char* b)
{
    int nb=strlen(b);
    return strncmp(a, b, nb)==0;
}

void Copy(char* a, const char* b, int n=0)
{
    if(n>0) {strncpy(a, b, n); a[n]=0;}
    else strcpy(a, b);
}

void AllocateAndCopy(char** a, const char* b)
{
    if(b==0) {*a=0; return;}
    int n=strlen(b);
    *a=new char[n+1];
    strcpy(*a, b);
}


// Input and Output //

#define MAX_LINE_LENGTH 10000

struct InFile
{
    FILE* file;
    int cur_line_num;

    char line_buf[MAX_LINE_LENGTH];
    int cur_ind, cur_line_size;

    InFile(const char* str) {file=0; if(str) file=fopen(str, "r"); cur_line_size=0; cur_ind=0; cur_line_num=0;}
    ~InFile(){if(file) fclose(file);} //taking inputs from file

    void SkipSpaces() //function to skip the spaces in the file
    {
        while(cur_ind<cur_line_size)
        {
            char ch=line_buf[cur_ind];
            if(ch!=' ' && ch!='\t' && ch!='\r' && ch!='\n') break;
            cur_ind++;
        }
    }

    bool SkipUpto(const char* str)
    {
        while(true)
        {
            SkipSpaces();
            while(cur_ind>=cur_line_size) {if(!GetNewLine()) return false; SkipSpaces();}

            if(StartsWith(&line_buf[cur_ind], str))
            {
                cur_ind+=strlen(str);
                return true;
            }
            cur_ind++;
        }
        return false;
    }

    bool GetNewLine() //function to get new line
    {
        cur_ind=0; line_buf[0]=0;
        if(!fgets(line_buf, MAX_LINE_LENGTH, file)) return false; //means it is empty
        cur_line_size=strlen(line_buf);
        if(cur_line_size==0) return false; //means we reached the end of the file
        cur_line_num++;
        return true;
    }

    char* GetNextTokenStr() //function to get the next token
    {
        SkipSpaces();
        while(cur_ind>=cur_line_size) {if(!GetNewLine()) return 0; SkipSpaces();}
        return &line_buf[cur_ind]; //return the next token
    }

    void Advance(int num)
    {
        cur_ind+=num;
    }
};

struct OutFile //File output
{
    FILE* file;
    OutFile(const char* str) {file=0; if(str) file=fopen(str, "w");}
    ~OutFile(){if(file) fclose(file);}

    void Out(const char* s)
    {
        fprintf(file, "%s\n", s); fflush(file);
    }
};


// Compiler Parameters //

struct CompilerInfo
{
    InFile in_file;
    OutFile out_file;
    OutFile debug_file;

    CompilerInfo(const char* in_str, const char* out_str, const char* debug_str)
                : in_file(in_str), out_file(out_str), debug_file(debug_str)
    {
    }
};


// Scanner //

#define MAX_TOKEN_LEN 40

enum TokenType{
                IF, THEN, ELSE, END, REPEAT, UNTIL, READ, WRITE,
                ASSIGN, EQUAL, LESS_THAN,
                PLUS, MINUS, TIMES, DIVIDE, POWER,
                SEMI_COLON,
                LEFT_PAREN, RIGHT_PAREN,
                LEFT_BRACE, RIGHT_BRACE,
                ID, NUM,
                ENDFILE, ERROR
              };


const char* TokenTypeStr[]=
            {
                "If", "Then", "Else", "End", "Repeat", "Until", "Read", "Write",
                "Assign", "Equal", "LessThan",
                "Plus", "Minus", "Times", "Divide", "Power",
                "SemiColon",
                "LeftParen", "RightParen",
                "LeftBrace", "RightBrace",
                "ID", "Num",
                "EndFile", "Error"
            };

struct Token
{
    TokenType type;
    char str[MAX_TOKEN_LEN+1];

    Token(){str[0]=0; type=ERROR;}
    Token(TokenType _type, const char* _str) {type=_type; Copy(str, _str);}
};

const Token reserved_words[]=
{
    Token(IF, "if"),
    Token(THEN, "then"),
    Token(ELSE, "else"),
    Token(END, "end"),
    Token(REPEAT, "repeat"),
    Token(UNTIL, "until"),
    Token(READ, "read"),
    Token(WRITE, "write")
};
const int num_reserved_words=sizeof(reserved_words)/sizeof(reserved_words[0]);


const Token symbolic_tokens[]=
{
    Token(ASSIGN, ":="),
    Token(EQUAL, "="),
    Token(LESS_THAN, "<"),
    Token(PLUS, "+"),
    Token(MINUS, "-"),
    Token(TIMES, "*"),
    Token(DIVIDE, "/"),
    Token(POWER, "^"),
    Token(SEMI_COLON, ";"),
    Token(LEFT_PAREN, "("),
    Token(RIGHT_PAREN, ")"),
    Token(LEFT_BRACE, "{"),
    Token(RIGHT_BRACE, "}")
};
const int num_symbolic_tokens=sizeof(symbolic_tokens)/sizeof(symbolic_tokens[0]);

inline bool IsDigit(char ch){return (ch>='0' && ch<='9');}
inline bool IsLetter(char ch){return ((ch>='a' && ch<='z') || (ch>='A' && ch<='Z'));}
inline bool IsLetterOrUnderscore(char ch){return (IsLetter(ch) || ch=='_');}

void GetNextToken(CompilerInfo* pci, Token* ptoken)
{
    ptoken->type=ERROR;
    ptoken->str[0]=0;

    int i;
    char* s=pci->in_file.GetNextTokenStr();
    if(!s)
    {
        ptoken->type=ENDFILE;
        ptoken->str[0]=0;
        return;
    }

    for(i=0;i<num_symbolic_tokens;i++)
    {
        if(StartsWith(s, symbolic_tokens[i].str))
            break;
    }

    if(i<num_symbolic_tokens)
    {
        if(symbolic_tokens[i].type==LEFT_BRACE)
        {
            pci->in_file.Advance(strlen(symbolic_tokens[i].str));
            if(!pci->in_file.SkipUpto(symbolic_tokens[i+1].str)) return;
            return GetNextToken(pci, ptoken);
        }
        ptoken->type=symbolic_tokens[i].type;
        Copy(ptoken->str, symbolic_tokens[i].str);
    }
    else if(IsDigit(s[0]))
    {
        int j=1;
        while(IsDigit(s[j])) j++;

        ptoken->type=NUM;
        Copy(ptoken->str, s, j);
    }
    else if(IsLetterOrUnderscore(s[0]))
    {
        int j=1;
        while(IsLetterOrUnderscore(s[j])) j++;

        ptoken->type=ID;
        Copy(ptoken->str, s, j);

        for(i=0;i<num_reserved_words;i++)
        {
            if(Equals(ptoken->str, reserved_words[i].str))
            {
                ptoken->type=reserved_words[i].type;
                break;
            }
        }
    }

    int len=strlen(ptoken->str);
    if(len>0) pci->in_file.Advance(len);
}

void scanningbegin(CompilerInfo* cio){ //Scanning done

    Token tobj;

    cout<<("Scanning")<<endl;
    cout<<endl;
    cio->out_file.Out("Scanning");
    while(true){
        GetNextToken(cio,&tobj);
        cout<<"["<<cio->in_file.cur_line_num<<"] "<<tobj.str<<" ["<<TokenTypeStr[tobj.type]<<"]";

        // This to write in file
        string ts="["+to_string(cio->in_file.cur_line_num)+"] "+tobj.str+" ["+TokenTypeStr[tobj.type]+"]"; //this is to convert number to a string
        char const* char_array= ts.c_str(); // this is to convert the string to be acharacter
        cio->out_file.Out(char_array);

        cout<<endl;
        if(tobj.type==ENDFILE){   //if it reached to the end of the file so From here it stop scanning
            cout<<endl;
            break;
        }
    }
}

// Parser //

// program -> stmtseq
// stmtseq -> stmt { ; stmt }
// stmt -> ifstmt | repeatstmt | assignstmt | readstmt | writestmt
// ifstmt -> if exp then stmtseq [ else stmtseq ] end
// repeatstmt -> repeat stmtseq until expr
// assignstmt -> identifier := expr
// readstmt -> read identifier
// writestmt -> write expr
// expr -> mathexpr [ (<|=) mathexpr ]
// mathexpr -> term { (+|-) term }    left associative
// term -> factor { (*|/) factor }    left associative
// factor -> newexpr { ^ newexpr }    right associative
// newexpr -> ( mathexpr ) | number | identifier

enum NodeKind{
                IF_NODE, REPEAT_NODE, ASSIGN_NODE, READ_NODE, WRITE_NODE,
                OPER_NODE, NUM_NODE, ID_NODE
             };

// Used for debugging only /////////////////////////////////////////////////////////
const char* NodeKindStr[]=
            {
                "If", "Repeat", "Assign", "Read", "Write",
                "Oper", "Num", "ID"
            };

enum ExprDataType {VOID, INTEGER, BOOLEAN};

// Used for debugging only /////////////////////////////////////////////////////////
const char* ExprDataTypeStr[]=
            {
                "Void", "Integer", "Boolean"
            };

#define MAX_CHILDREN 3

struct TreeNode
{
    TreeNode* child[MAX_CHILDREN];
    TreeNode* sibling; // used for sibling statements only

    NodeKind node_kind;

    union{TokenType oper; int num; char* id;}; // defined for expression/int/identifier only
    ExprDataType expr_data_type; // defined for expression/int/identifier only

    int line_num;

    TreeNode() {int i; for(i=0;i<MAX_CHILDREN;i++) child[i]=0; sibling=0; expr_data_type=VOID;}
};

struct ParseInfo
{
    Token next_token;
};

void Ismatching(CompilerInfo* co, ParseInfo* po, TokenType expectedT)
{
    co->debug_file.Out("Start Match"); //it starts the match

    if(po->next_token.type!=expectedT) throw 0; //throw exception
    GetNextToken(co, &po->next_token); //oit get the next token

    fprintf(co->debug_file.file, "[%d] %s (%s)\n", co->in_file.cur_line_num, po->next_token.str, TokenTypeStr[po->next_token.type]); fflush(co->debug_file.file);
}

TreeNode* mathexprcalc(CompilerInfo*, ParseInfo*); //it claculates the expression

// newexpr -> ( mathexpr ) | number | identifier
TreeNode* newexpr(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start NewExpr"); //it dtarts the new ecpression

    //It Compares the next token that I have with the First() of possible statements
    if(po->next_token.type==NUM) //check if it is num type
    {
        TreeNode* trn=new TreeNode;
        trn->node_kind=NUM_NODE; //let it to the node of num
        trn->expr_data_type= INTEGER; //let the datatype of it to integer
        char* num_str=po->next_token.str;
        trn->num=0; while(*num_str) trn->num=trn->num*10+((*num_str++)-'0');
        trn->line_num=co->in_file.cur_line_num;
        Ismatching(co, po, po->next_token.type);//perform the function to check the matching

        co->debug_file.Out("End NewExpr"); //it iss end of the new expression
        return trn;
    }

    if(po->next_token.type==ID) //check if it is ID or not
    {
        TreeNode* trn=new TreeNode;
        trn->node_kind=ID_NODE; //let it to the node of id
        trn->expr_data_type=INTEGER;  //let the datatype of it to integer
        AllocateAndCopy(&trn->id, po->next_token.str); //perform the allocate and copy
        trn->line_num=co->in_file.cur_line_num;
        Ismatching(co, po, po->next_token.type); //perform the function to check the matching
        co->debug_file.Out("End NewExpr"); //it is the end of the new expression
        return trn;
    }

    if(po->next_token.type==LEFT_PAREN) // check if it is Left PAREN or not
    {
        Ismatching(co, po, LEFT_PAREN); //perform the function to check the left paren
        TreeNode* trn=mathexprcalc(co, po);
        Ismatching(co, po, RIGHT_PAREN); //perform the function to check the right paren

        co->debug_file.Out("End NewExpr"); //it is the end of the new expression
        return trn;
    }

    throw 0; //throw exception
    return 0;
}

// factor -> newexpr { ^ newexpr }    right associative
TreeNode* factor(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start Factor"); //it starts the factor

    TreeNode* trn=newexpr(co, po);

    if(po->next_token.type==POWER) //it see if the type in power or not
    {
        TreeNode* newtrn=new TreeNode;
        newtrn->node_kind=OPER_NODE;
        newtrn->oper=po->next_token.type; //get the next type
        newtrn->expr_data_type=INTEGER; //let the type be integer

        newtrn->line_num=co->in_file.cur_line_num;

        newtrn->child[0]=trn;
        Ismatching(co, po, po->next_token.type); //call the function matching to check on it
        newtrn->child[1]=factor(co, po);

        co->debug_file.Out("End Factor"); //the end of the factor
        return newtrn;
    }
    co->debug_file.Out("End Factor"); //so this will be the end
    return trn;
}

// term -> factor { (*|/) factor }    left associative
TreeNode* term(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start Term"); //it starts the term

    TreeNode* trn=factor(co, po);

    while(po->next_token.type==TIMES || po->next_token.type==DIVIDE) //the while loop will continou if the type is times or divide
    {
        TreeNode* newtrn=new TreeNode;
        newtrn->node_kind=OPER_NODE;
        newtrn->oper=po->next_token.type; //get the next type
        newtrn->expr_data_type=INTEGER; //let the type be integer

        newtrn->line_num=co->in_file.cur_line_num;

        newtrn->child[0]=trn;
        Ismatching(co, po, po->next_token.type); //get next token
        newtrn->child[1]=factor(co, po);

        trn=newtrn;
    }
    co->debug_file.Out("End Term"); //if reached the end of the file it means it it the END
    return trn;
}

// mathexpr -> term { (+|-) term }    left associative
TreeNode* mathexprcalc(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start MathExpr"); ///it starts the math exp

    TreeNode* trn=term(co, po);

    while(po->next_token.type==PLUS || po->next_token.type==MINUS) //the while loop will continou if the type is + or -

    {
        TreeNode* newtrn=new TreeNode;
        newtrn->node_kind=OPER_NODE;
        newtrn->oper=po->next_token.type; //get the next type
        newtrn->line_num=co->in_file.cur_line_num;
        newtrn->expr_data_type=INTEGER; //let the type is integer
        newtrn->child[0]=trn;
        Ismatching(co, po, po->next_token.type);
        newtrn->child[1]=term(co, po);

        trn=newtrn;
    }
    co->debug_file.Out("End MathExpr"); //it is the end of expression
    return trn;
}

// expr -> mathexpr [ (<|=) mathexpr ]
TreeNode* expr(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start Expr"); //starts the expression

    TreeNode* trn=mathexprcalc(co, po);

    if(po->next_token.type==EQUAL || po->next_token.type==LESS_THAN) //the while loop will continou if the type is= or < less

    {
        TreeNode* newtrn=new TreeNode;
        newtrn->node_kind=OPER_NODE; //it let the kind is oper node
        newtrn->oper=po->next_token.type;
        newtrn->expr_data_type = BOOLEAN; //let the type to be Boolean
        newtrn->line_num=co->in_file.cur_line_num;

        newtrn->child[0]=trn;
        Ismatching(co, po, po->next_token.type);
        newtrn->child[1]=mathexprcalc(co, po); //perform the calculation of math expression

        co->debug_file.Out("End Expr"); //it is the end of the expression
        return newtrn;
    }
    co->debug_file.Out("End Expr"); //it is the end of the expression
    return trn;
}

// writestmt -> write expr
TreeNode* writestmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start WriteStmt"); //it starts write the stmt

    TreeNode* trn=new TreeNode;
    trn->node_kind=WRITE_NODE;
    trn->expr_data_type=VOID; //let the type be void
    trn->line_num=co->in_file.cur_line_num;

    Ismatching(co, po, WRITE); //check the matching
    trn->child[0]=expr(co, po);

    co->debug_file.Out("End WriteStmt"); //the end of write stmt
    return trn;
}

// readstmt -> read identifier
TreeNode* readstmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start ReadStmt"); //it starts the read stmt

    TreeNode* trn=new TreeNode;
    trn->node_kind=READ_NODE;
    trn->expr_data_type=VOID; //let the type is void
    trn->line_num=co->in_file.cur_line_num;

    Ismatching(co, po, READ); //call the Ismatching function to check
    if(po->next_token.type==ID) AllocateAndCopy(&trn->id, po->next_token.str); //perform allocate and copy
    Ismatching(co, po, ID);

    co->debug_file.Out("End ReadStmt"); //the end of read stmt
    return trn;
}

//assignstmt -> identifier := expr
TreeNode* assignstmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start AssignStmt"); //start assign stmt

    TreeNode* trn=new TreeNode;
    trn->node_kind=ASSIGN_NODE; //let the node kind assign
    trn->line_num=co->in_file.cur_line_num;

    if(po->next_token.type==ID) AllocateAndCopy(&trn->id, po->next_token.str); //perform allocate and copy
    Ismatching(co, po, ID);
    Ismatching(co, po, ASSIGN); trn->child[0]=expr(co, po); //call Ismatching function

    co->debug_file.Out("End AssignStmt"); //end of assign stmt
    return trn;
}

TreeNode* stmtseq(CompilerInfo*, ParseInfo*);

// repeatstmt -> repeat stmtseq until expr
TreeNode* repeatstmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start RepeatStmt");

    TreeNode* trn=new TreeNode;
    trn->node_kind=REPEAT_NODE;
    trn->expr_data_type=VOID; //let the type void
    trn->line_num=co->in_file.cur_line_num;

    Ismatching(co, po, REPEAT); trn->child[0]=stmtseq(co, po);
    Ismatching(co, po, UNTIL); trn->child[1]=expr(co, po);

    co->debug_file.Out("End RepeatStmt"); //end of repeat stmt
    return trn;
}

// ifstmt -> if exp then stmtseq [ else stmtseq ] end
TreeNode* ifstmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start IfStmt"); //ifstmt starts

    TreeNode* trn=new TreeNode;
    trn->node_kind=IF_NODE;
    trn->expr_data_type=VOID; //let the type to be void
    trn->line_num=co->in_file.cur_line_num;

    Ismatching(co, po, IF); trn->child[0]=expr(co, po);
    Ismatching(co, po, THEN); trn->child[1]=stmtseq(co, po);
    if(po->next_token.type==ELSE) {Ismatching(co, po, ELSE); trn->child[2]=stmtseq(co, po);}
    Ismatching(co, po, END);

    co->debug_file.Out("End IfStmt"); //the end of stmt
    return trn;
}

// stmt -> ifstmt | repeatstmt | assignstmt | readstmt | writestmt
TreeNode* stmt(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start Stmt"); //starts the stmt

    TreeNode* trn=0;
    //it checks for the type of the token  to call the function
    if(po->next_token.type==IF) trn=ifstmt(co, po);
    else if(po->next_token.type==REPEAT) trn=repeatstmt(co, po);
    else if(po->next_token.type==ID) trn=assignstmt(co, po);
    else if(po->next_token.type==READ) trn=readstmt(co, po);
    else if(po->next_token.type==WRITE) trn=writestmt(co, po);
    else throw 0;

    co->debug_file.Out("End Stmt"); //the end of stmt
    return trn;
}

// stmtseq -> stmt { ; stmt }
TreeNode* stmtseq(CompilerInfo* co, ParseInfo* po)
{
    co->debug_file.Out("Start StmtSeq"); //the stmt starts

    TreeNode* firsttrn=stmt(co, po);
    TreeNode* lasttrn=firsttrn;

    while(po->next_token.type!=ENDFILE && po->next_token.type!=END &&
          po->next_token.type!=ELSE && po->next_token.type!=UNTIL)
    {
        Ismatching(co, po, SEMI_COLON); //see if it is semicolon or not
        TreeNode* nexttrn=stmt(co, po); //then it will make a new treenode
        lasttrn->sibling=nexttrn;
        lasttrn=nexttrn;
    }

    co->debug_file.Out("End StmtSeq"); //the end of stmt seq
    return firsttrn;
}

// program -> stmtseq
TreeNode* Parse(CompilerInfo* co)  //parser function
{
    ParseInfo po;
    GetNextToken(co, &po.next_token);

    TreeNode* treestructure=stmtseq(co, &po);

    if(po.next_token.type!=ENDFILE)
    co->debug_file.Out("Error, The file is at the end"); //it will print this error message

    return treestructure;
}

void PrintTree(TreeNode* N, int sh=0) //it display the tree
{
    int i, NSH=3;
    for(i=0;i<sh;i++) printf(" ");

    printf("[%s]", NodeKindStr[N->node_kind]);

    if(N->node_kind==OPER_NODE) printf("[%s]", TokenTypeStr[N->oper]); //display the structure of the tree
    else if(N->node_kind==NUM_NODE) printf("[%d]", N->num);
    else if(N->node_kind==ID_NODE || N->node_kind==READ_NODE || N->node_kind==ASSIGN_NODE) printf("[%s]", N->id);

    if(N->expr_data_type!=VOID) printf("[%s]", ExprDataTypeStr[N->expr_data_type]); //here to print the datatype

    printf("\n");

    for(i=0;i<MAX_CHILDREN;i++) if(N->child[i]) PrintTree(N->child[i], sh+NSH);
    if(N->sibling) PrintTree(N->sibling, sh);//display siblings
}

void removetree(TreeNode* N) //remove the tree at all
{
    int i;

    if(N->node_kind==ID_NODE || N->node_kind==READ_NODE || N->node_kind==ASSIGN_NODE) //see if it is ID or READ or ASSIGN node
        if(N->id) delete[] N->id; //if it is ID so it will delete it

    for(i=0;i<MAX_CHILDREN;i++) if(N->child[i]) removetree(N->child[i]);
    if(N->sibling) removetree(N->sibling); //here it remove the siblings

    delete N;
}

void parsingprocess(CompilerInfo* t){  //parsing done here
     TreeNode* treestructure = new TreeNode;
     treestructure = Parse(t);
     cout<<("Parsing")<<endl;
     PrintTree(treestructure); //print the structure of the tree
     cout<<endl;
}

const int SYMBOL_HASH_SIZE=10007;

struct LineLocation
{
    int line_num;
    LineLocation* next;
};

struct VariableInfo
{
    char* name;
    int memloc;
    LineLocation* head_line; // the head of linked list of source line locations
    LineLocation* tail_line; // the tail of linked list of source line locations
    VariableInfo* next_var; // the next variable in the linked list in the same hash bucket of the symbol table
};

struct SymbolTable
{
    int num_vars;
    VariableInfo* var_info[SYMBOL_HASH_SIZE];

    SymbolTable() {num_vars=0; int i; for(i=0;i<SYMBOL_HASH_SIZE;i++) var_info[i]=0;}

    int Hash(const char* name)
    {
        int i, len=strlen(name);
        int hash_val=11;
        for(i=0;i<len;i++) hash_val=(hash_val*17+(int)name[i])%SYMBOL_HASH_SIZE;
        return hash_val;
    }

    VariableInfo* Find(const char* name)
    {
        int h=Hash(name);
        VariableInfo* cur=var_info[h];
        while(cur)
        {
            if(Equals(name, cur->name)) return cur;
            cur=cur->next_var;
        }
        return 0;
    }

    void Insert(const char* name, int line_num)
    {
        LineLocation* lineloc=new LineLocation;
        lineloc->line_num=line_num;
        lineloc->next=0;

        int h=Hash(name);
        VariableInfo* prev=0;
        VariableInfo* cur=var_info[h];

        while(cur)
        {
            if(Equals(name, cur->name))
            {
                // just add this line location to the list of line locations of the existing var
                cur->tail_line->next=lineloc;
                cur->tail_line=lineloc;
                return;
            }
            prev=cur;
            cur=cur->next_var;
        }

        VariableInfo* vi=new VariableInfo;
        vi->head_line=vi->tail_line=lineloc;
        vi->next_var=0;
        vi->memloc=num_vars++;
        AllocateAndCopy(&vi->name, name);

        if(!prev) var_info[h]=vi;
        else prev->next_var=vi;
    }

    void Print()
    {
        int i;
        for(i=0;i<SYMBOL_HASH_SIZE;i++)
        {
            VariableInfo* curv=var_info[i];
            while(curv)
            {
                printf("[Var=%s][Mem=%d]", curv->name, curv->memloc);
                LineLocation* curl=curv->head_line;
                while(curl)
                {
                    printf("[Line=%d]", curl->line_num);
                    curl=curl->next;
                }
                printf("\n");
                curv=curv->next_var;
            }
        }
    }

    void Destroy()
    {
        int i;
        for(i=0;i<SYMBOL_HASH_SIZE;i++)
        {
            VariableInfo* curv=var_info[i];
            while(curv)
            {
                LineLocation* curl=curv->head_line;
                while(curl)
                {
                    LineLocation* pl=curl;
                    curl=curl->next;
                    delete pl;
                }
                VariableInfo* p=curv;
                curv=curv->next_var;
                delete p;
            }
            var_info[i]=0;
        }
    }
};

// Check if node is an ID, READ, or ASSIGN node
bool isVariableNode(TreeNode *node) {
    return (node->node_kind == ID_NODE || node->node_kind == READ_NODE || node->node_kind == ASSIGN_NODE);
}
void constructSymbolTable(TreeNode *node, SymbolTable *symbol_table)
{
    // Add variable to symbol table if contains variable
    if (isVariableNode(node)) {
        symbol_table->Insert(node->id, node->line_num);
    }

    // loop on node's children
    for (int i = 0; i < MAX_CHILDREN; i++)
        if (node->child[i])
            constructSymbolTable(node->child[i], symbol_table);

    // if node has a sibling
    if (node->sibling)
        constructSymbolTable(node->sibling, symbol_table);
}


int execute(TreeNode *node, SymbolTable *symbol_table, int *memory) {
    if (node->node_kind == NUM_NODE) {
        // Return the number directly
        return node->num;
    } else if (node->node_kind == ID_NODE) {
        // Retrieve the variable's value from memory using symbol table
        int var_loc = symbol_table->Find(node->id)->memloc;
        return memory[var_loc];
    } else {
        // Execute operation for non-numeric and non-variable nodes
        int a = execute(node->child[0], symbol_table, memory);
        int b = execute(node->child[1], symbol_table, memory);
        switch (node->oper) {
            case EQUAL: return a == b;
            case LESS_THAN: return a < b;
            case PLUS: return a + b;
            case MINUS: return a - b;
            case TIMES: return a * b;
            case DIVIDE: return a / b;
            case POWER: return pow(a, b);
            default: throw "Invalid operation"; // Throwing an exception for an unknown operation
        }
    }
}


// Run
void run(TreeNode *node, SymbolTable *symbol_table, int *memory)
{
    switch (node->node_kind)
    {
        case IF_NODE:
        {
            int condition = execute(node->child[0], symbol_table, memory);
            if (condition)
                // run on if body
                run(node->child[1], symbol_table, memory);
            else if (node->child[2])
                // run on else body
                run(node->child[2], symbol_table, memory);
            break;
        }
        case ASSIGN_NODE:
        {
            // assign value to variable in memory
            int v = execute(node->child[0], symbol_table, memory);
            memory[symbol_table->Find(node->id)->memloc] = v;
            break;
        }
        case READ_NODE:
        {
            cout << "Enter " << node->id << ": ";
            int val = 0;
            cin >> val;
            memory[symbol_table->Find(node->id)->memloc] = val;
            break;
        }
        case WRITE_NODE:
        {
            // the value of the variable
            int v = execute(node->child[0], symbol_table, memory);
            cout << "val: " << v << endl;
            break;
        }
        case REPEAT_NODE:
        {
            do {
                run(node->child[0], symbol_table, memory);
            } while (!execute(node->child[1], symbol_table, memory));
            break;
        }
        default: break;
    }

    // run on node sibling if exists
    if (node->sibling)
        run(node->sibling, symbol_table, memory);
}

int* initMemory(int size) {
    int *memory = new int[size];
    for (int i = 0; i < size; i++)
        memory[i] = 0;

    return memory;
}

void deallocateMemoryArray(const int* memoryArray) {
    delete[] memoryArray;
}
int main()
{
    CompilerInfo *pci = new CompilerInfo("input.txt", "out.txt", "debug.txt");

    // Parsing Tree
    TreeNode *syntax_tree = Parse(pci);

    // Construct Symbol Tree
    SymbolTable symbol_table;
    constructSymbolTable(syntax_tree, &symbol_table);

    // Print Symbol Table
    cout << "-- Symbol Table:" << endl;
    symbol_table.Print();
    cout << "\n" << endl;

    // Print Syntax Tree
    cout << "-- Syntax Tree:" << endl;
    PrintTree(syntax_tree);
    cout << "\n" << endl;

    // Initialize memory with the number of variables
    int *memory = initMemory(symbol_table.num_vars);

    // Simulation of the compilation
    cout << "-- Compilation: " << endl;
    run(syntax_tree, &symbol_table, memory);
    cout << "\n" << endl;

    // destruction
    symbol_table.Destroy();
    removetree(syntax_tree);
    deallocateMemoryArray(memory);

    return 0;
}
