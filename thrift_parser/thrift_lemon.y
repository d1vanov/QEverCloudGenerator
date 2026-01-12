/* http://thrift.apache.org/docs/idl/ */
/* http://www.hwaci.com/sw/lemon/lemon.html */

%stack_size 2000

%include {
#include "Parser.h"
#include "ParserHelper.h"
#include <QDebug>
#include <assert.h>
#include <utility>
}

%token_prefix TERM_

%extra_argument { qevercloud_generator::Parser* pParser }

%token_type    { QString* }
%token_destructor {delete $$;}

%syntax_error
{
    pParser->setErrorFlag(QStringLiteral("Syntax error"));
}
%parse_failure
{
    pParser->setErrorFlag(QStringLiteral("Parser failure"));
}
%stack_overflow
{
    pParser->setErrorFlag(QStringLiteral("Stack overflow"));
}

thrift ::= body END_OF_FILE.
body ::= .
body ::= include body.
body ::= cppinclude body.
body ::= namespace body.
body ::= const body.
body ::= typedef body.
body ::= enum body.
body ::= struct body.
body ::= union body.
body ::= exception body.
body ::= service body.

include ::=  INCLUDE STRING_VALUE(A).
{
  pParser->addInclude(*A);
  delete A;
}

cppinclude ::=  CPP_INCLUDE STRING_VALUE.

namespace  ::=  NAMESPACE IDENTIFIER(A) IDENTIFIER(B).
{
  pParser->addNamespace(*A, *B);
  delete A;
  delete B;
}

const      ::= constbody .
const      ::= constbody LISTSEP .
constbody  ::= DOC_COMMENT(D) CONST fieldtype(A) IDENTIFIER(B) EQ constvalue(C) .
{
  std::shared_ptr<qevercloud_generator::Parser::Type> t = A->type();
  pParser->addConst(t, *B, std::shared_ptr<qevercloud_generator::Parser::ConstValue>(C), *D);
  delete A;
  delete B;
  delete D;
}
constbody  ::= CONST fieldtype(A) IDENTIFIER(B) EQ constvalue(C) .
{
  std::shared_ptr<qevercloud_generator::Parser::Type> t = A->type();
  pParser->addConst(t, *B, std::shared_ptr<qevercloud_generator::Parser::ConstValue>(C), QString());
  delete A;
  delete B;
}


typedef    ::= DOC_COMMENT(D) TYPEDEF definitiontype(B) IDENTIFIER(C).
{
  std::shared_ptr<qevercloud_generator::Parser::Type> t = B->type();
  pParser->addTypedef(*C, t, *D);
  delete B;
  delete C;
  delete D;
}

typedef    ::= TYPEDEF definitiontype(B) IDENTIFIER(C).
{
  std::shared_ptr<qevercloud_generator::Parser::Type> t = B->type();
  pParser->addTypedef(*C, t, QString());
  delete B;
  delete C;
}

enum       ::=  ENUM IDENTIFIER(A) CURLY_OPEN enumbody(B) CURLY_CLOSE.
{
  pParser->addEnumeration(*A, *B, QString());
  delete A;
  delete B;
}

enum       ::= DOC_COMMENT(D) ENUM IDENTIFIER(A) CURLY_OPEN enumbody(B) CURLY_CLOSE.
{
  pParser->addEnumeration(*A, *B, *D);
  delete A;
  delete B;
  delete D;
}

%type enumbody {QList<std::pair<QString, QString>>*}
%destructor enumbody {delete $$;}
enumbody(A)    ::= .
{
  A = new QList<std::pair<QString, QString>>;
}
enumbody(A)    ::= enumitem(B) enumbody(C) .
{
  A = C;
  A->prepend(*B);
  delete B;
}

%type enumitem {std::pair<QString, QString>*}
%destructor enumitem {delete $$;}
enumitem(A)    ::= enumvalue(B) enumsep . { A = B;}
enumsep     ::= .
enumsep     ::= LISTSEP.

%type enumvalue {std::pair<QString, QString>*}
%destructor enumvalue {delete $$;}
enumvalue(A)   ::= IDENTIFIER(B) enuminit(C) .
{
  A = new std::pair<QString, QString>(*B, *C);
  delete B;
  delete C;
}

%type enuminit {QString*}
%destructor enuminit {delete $$;}
enuminit(A) ::= .
{
  A = new QString;
}
enuminit(A) ::= EQ INTEGER_VALUE(B) .
{
  A = B;
}

union      ::=  UNION IDENTIFIER(A) CURLY_OPEN fieldlist(B) CURLY_CLOSE.
{
  pParser->addUnion(*A, *B);
  delete A;
  delete B;
}


exception  ::=  EXCEPTION IDENTIFIER(A) CURLY_OPEN fieldlist(B) CURLY_CLOSE.
{
  pParser->addException(*A, *B, QString());
  delete A;
  delete B;
}
exception  ::= DOC_COMMENT(D) EXCEPTION IDENTIFIER(A) CURLY_OPEN fieldlist(B) CURLY_CLOSE.
{
  pParser->addException(*A, *B, *D);
  delete A;
  delete B;
  delete D;
}


struct     ::= STRUCT IDENTIFIER(A) CURLY_OPEN fieldlist(B) CURLY_CLOSE.
{
  pParser->addStructure(*A, *B, QString());
  delete A;
  delete B;
}
struct     ::= DOC_COMMENT(D) STRUCT IDENTIFIER(A) CURLY_OPEN fieldlist(B) CURLY_CLOSE.
{
  pParser->addStructure(*A, *B, *D);
  delete A;
  delete B;
  delete D;
}

%type fieldlist {QList<qevercloud_generator::Parser::Field>*}
%destructor fieldlist {delete $$;}
fieldlist(A)  ::= .
{
  A = new QList<qevercloud_generator::Parser::Field>;
}
fieldlist(A)  ::= fieldlist(B) field(C).
{
  A = B;
  A->append(*C);
  delete C;
}

%type field {qevercloud_generator::Parser::Field*}
%destructor field {delete $$;}
field(A) ::= fieldbody(B) . {A = B;}
field(A) ::= fieldbody(B) LISTSEP . {A = B;}

%type fieldbody {qevercloud_generator::Parser::Field*}
%destructor fieldbody {delete $$;}
fieldbody(A) ::= fieldidoption(B) fieldreq(C) fieldtype(D) IDENTIFIER(E) fieldinitializer(F) .
{
  A = new qevercloud_generator::Parser::Field;
  A->m_id = B;
  A->m_required = C;
  A->m_affiliation = qevercloud_generator::Parser::Field::Affiliation::Evernote;
  A->m_type = std::shared_ptr<qevercloud_generator::Parser::Type>(D->type());
  A->m_name = *E;
  A->m_initializer = std::shared_ptr<qevercloud_generator::Parser::ConstValue>(F);
  delete E;
}

%type fieldidoption {int}
fieldidoption(A) ::= .
{
  A = -1;
}
fieldidoption(A) ::= fieldid(B).
{
  A = B;
}

%type fieldinitializer {qevercloud_generator::Parser::ConstValue*}
%destructor fieldinitializer {delete $$;}
fieldinitializer(A) ::= .
{
  A = nullptr;
}
fieldinitializer(A) ::= EQ constvalue(B).
{
  A = B;
}

%type fieldid {int}
fieldid(A)    ::=  INTEGER_VALUE(B) COLON.
{
  A = B->toInt();
  delete B;
}

%type fieldreq {qevercloud_generator::Parser::Field::RequiredFlag}
fieldreq(A)   ::= .
{
  A = qevercloud_generator::Parser::Field::RequiredFlag::Default;
}
fieldreq(A)   ::= REQUIRED.
{
  A = qevercloud_generator::Parser::Field::RequiredFlag::Required;
}
fieldreq(A)   ::= OPTIONAL.
{
  A = qevercloud_generator::Parser::Field::RequiredFlag::Optional;
}


service    ::=  SERVICE IDENTIFIER(A) extends(B) CURLY_OPEN functionlist(C) CURLY_CLOSE.
{
  pParser->addService(*A, B ? *B : QString(), *C, QString());
  delete A;
  delete B;
  delete C;
}
service    ::= DOC_COMMENT(D) SERVICE IDENTIFIER(A) extends(B) CURLY_OPEN functionlist(C) CURLY_CLOSE.
{
  pParser->addService(*A, B ? *B : QString(), *C, *D);
  delete A;
  delete B;
  delete C;
  delete D;
}

%type extends {QString*}
%destructor fieldtype {delete $$; }
extends(A)    ::= . {
  A = nullptr;
}
extends(A)    ::= EXTENDS IDENTIFIER(B) .
{
  A = B;
}

%type functionlist {QList<qevercloud_generator::Parser::Function>*}
%destructor functionlist {delete $$; }
functionlist(A) ::= .
{
  A = new QList<qevercloud_generator::Parser::Function>;
}
functionlist(A) ::= functionlist(B) function(C).
{
  A = B;
  B->append(*C);
  delete C;
}

%type function {qevercloud_generator::Parser::Function*}
%destructor function {delete $$; }
function(A) ::= functionbody(B). {A = B;}
function(A) ::= functionbody(B) LISTSEP. {A = B;}

%type functionbody {qevercloud_generator::Parser::Function*}
%destructor functionbody {delete $$; }

functionbody(A) ::= oneway(B) functiontype(C) IDENTIFIER(D) PAREN_OPEN fieldlist(E) PAREN_CLOSE throws(F).
{
  A = new qevercloud_generator::Parser::Function;
  A->m_isOneway = B;
  A->m_type = C->type();
  A->m_name = *D;
  A->m_params = *E;
  A->m_throws = *F;
  A->m_docComment = QLatin1String("");
  delete C;
  delete D;
  delete E;
  delete F;
}
functionbody(A) ::= DOC_COMMENT(X) oneway(B) functiontype(C) IDENTIFIER(D) PAREN_OPEN fieldlist(E) PAREN_CLOSE throws(F).
{
  A = new qevercloud_generator::Parser::Function;
  A->m_isOneway = B;
  A->m_type = C->type();
  A->m_name = *D;
  A->m_params = *E;
  A->m_throws = *F;
  A->m_docComment = *X;
  delete C;
  delete D;
  delete E;
  delete F;
  delete X;
}

%type oneway {bool}
oneway(A) ::= . {A = false;}
oneway(A) ::= ONEWAY. {A = true;}

%type functiontype {qevercloud_generator::Fieldtype*}
%destructor functiontype {delete $$;}
functiontype(A) ::=  fieldtype(B).
{
  A = B;
}
functiontype(A) ::=  VOID.
{
  A = new qevercloud_generator::VoidType;
}

%type throws {QList<qevercloud_generator::Parser::Field>*}
%destructor throws {delete $$;}
throws(A)  ::= .
{
  A = new QList<qevercloud_generator::Parser::Field>;
}
throws(A)  ::=  THROWS PAREN_OPEN fieldlist(B) PAREN_CLOSE.
{
  A = B;
}

%type fieldtype {qevercloud_generator::Fieldtype*}
%destructor fieldtype {delete $$; }
fieldtype(A)    ::=  IDENTIFIER(B).
{
  qevercloud_generator::IdentifierFieldType* p = new qevercloud_generator::IdentifierFieldType();
  p->m_identifier = *B;
  A = p;
  delete B;
}
fieldtype(A)    ::=  definitiontype(B) .
{
  A = B;
}


%type definitiontype {qevercloud_generator::DefinitionType*}
%destructor definitiontype {delete $$; }
definitiontype(A)  ::=  primitivetype(B) .
{
  qevercloud_generator::PrimitiveTypeDefinitionType* p = new qevercloud_generator::PrimitiveTypeDefinitionType();
  p->m_type = B;
  A = p;
}
definitiontype(A)  ::=  stringtype(B) .
{
  A = new qevercloud_generator::StringTypeDefinitionType;
  (void)B;
}
definitiontype(A)  ::=  bytearraytype(B) .
{
  A = new qevercloud_generator::ByteArrayDefinitionType();
  (void)B;
}
definitiontype(A)  ::=  containertype(B) .
{
  qevercloud_generator::ContainerTypeDefinitionType* p = new qevercloud_generator::ContainerTypeDefinitionType();
  p->m_containerType = B;
  A = p;
}

%type primitivetype {qevercloud_generator::Parser::PrimitiveType::Type}
primitivetype(A)   ::=  BOOL. {A = qevercloud_generator::Parser::PrimitiveType::Type::Bool;}
primitivetype(A)   ::=  BYTE. {A = qevercloud_generator::Parser::PrimitiveType::Type::Byte;}
primitivetype(A)   ::=  I16. {A = qevercloud_generator::Parser::PrimitiveType::Type::Int16;}
primitivetype(A)   ::=  I32. {A = qevercloud_generator::Parser::PrimitiveType::Type::Int32;}
primitivetype(A)   ::=  I64. {A = qevercloud_generator::Parser::PrimitiveType::Type::Int64;}
primitivetype(A)   ::=  DOUBLE. {A = qevercloud_generator::Parser::PrimitiveType::Type::Double;}

%type stringtype {void*}
stringtype(A)      ::=  STRING. {A = nullptr;}

%type bytearraytype {void*}
bytearraytype(A)   ::=  BINARY. {A = nullptr;}

%type containertype {qevercloud_generator::ContainerType*}
containertype(A)   ::=  maptype(B) .
{
  A = B;
}
containertype(A)   ::=  settype(B).
{
  A = B;
}
containertype(A)   ::=  listtype(B).
{
  A = B;
}

%type maptype {qevercloud_generator::MapType*}
%destructor maptype {delete $$;}
maptype(A)         ::=  MAP LT fieldtype(B) LISTSEP fieldtype(C) GT.
{
  A = new qevercloud_generator::MapType();
  A->m_keyType = B->type();
  A->m_valueType = C->type();
  delete B;
  delete C;
}

%type settype {qevercloud_generator::SetType*}
%destructor settype {delete $$;}
settype(A)         ::=  SET LT fieldtype(B) GT.
{
  A = new qevercloud_generator::SetType();
  A->m_valueType = B->type();
  delete B;
}

%type listtype {qevercloud_generator::ListType*}
%destructor listtype {delete $$;}
listtype(A)         ::=  LIST LT fieldtype(B) GT.
{
  A = new qevercloud_generator::ListType();
  A->m_valueType = B->type();
  delete B;
}

%type constvalue {qevercloud_generator::Parser::ConstValue*}
%destructor constvalue {delete $$;}
constvalue(A)    ::=  INTEGER_VALUE(B) .
{
  qevercloud_generator::Parser::IntegerValue* p = new qevercloud_generator::Parser::IntegerValue;
  p->m_value = *B;
  A = p;
  delete B;
}
constvalue(A)    ::=  DOUBLE_VALUE(B) .
{
  qevercloud_generator::Parser::DoubleValue* p = new qevercloud_generator::Parser::DoubleValue;
  p->m_value = *B;
  A = p;
  delete B;
}
constvalue(A)    ::=  STRING_VALUE(B) .
{
  qevercloud_generator::Parser::StringValue* p = new qevercloud_generator::Parser::StringValue;
  p->m_value = *B;
  A = p;
  delete B;
}
constvalue(A)    ::=  IDENTIFIER(B) .
{
  qevercloud_generator::Parser::IdentifierValue* p = new qevercloud_generator::Parser::IdentifierValue;
  p->m_value = *B;
  A = p;
  delete B;
}
constvalue(A)    ::=  constlist(B) .
{
  qevercloud_generator::Parser::ListValue* p = new qevercloud_generator::Parser::ListValue;
  p->m_values = *B;
  A = p;
  delete B;
}
constvalue(A)    ::=  constmap(B) .
{
  qevercloud_generator::Parser::MapValue* p = new qevercloud_generator::Parser::MapValue;
  p->m_values = *B;
  A = p;
  delete B;
}

%type constlist {QList<std::shared_ptr<qevercloud_generator::Parser::ConstValue>>*}
%destructor constvaluelist {delete $$;}
%type constvaluelist {QList<std::shared_ptr<qevercloud_generator::Parser::ConstValue>>*}
%destructor constvaluelist {delete $$;}
constlist(A)     ::=  BRACKET_OPEN constvaluelist(B) BRACKET_CLOSE .
{
  A = B;
}
constvaluelist(A) ::= .
{
  A = new QList<std::shared_ptr<qevercloud_generator::Parser::ConstValue>>;
}
constvaluelist(A) ::= constvalue(B) constvaluelist(C).
{
  A = C;
  A->prepend(std::shared_ptr<qevercloud_generator::Parser::ConstValue>(B));
}
constvaluelist(A) ::= constvalue(B) LISTSEP constvaluelist(C).
{
  A = C;
  A->prepend(std::shared_ptr<qevercloud_generator::Parser::ConstValue>(B));
}

%type constmap {QList<std::pair<std::shared_ptr<qevercloud_generator::Parser::ConstValue>, std::shared_ptr<qevercloud_generator::Parser::ConstValue>>>*}
%destructor constmap {delete $$;}
%type constmapvaluelist {QList<std::pair<std::shared_ptr<qevercloud_generator::Parser::ConstValue>, std::shared_ptr<qevercloud_generator::Parser::ConstValue>>>*}
%destructor constmapvaluelist {delete $$;}
constmap(A)     ::=  CURLY_OPEN constmapvaluelist(B) CURLY_CLOSE .
{
  A = B;
}
constmapvaluelist(A) ::= .
{
  A = new QList<std::pair<std::shared_ptr<qevercloud_generator::Parser::ConstValue>, std::shared_ptr<qevercloud_generator::Parser::ConstValue>>>;
}
constmapvaluelist(A) ::= constvalue(B) COLON constvalue(C) constmapvaluelist(D).
{
  A = D;
  auto p = std::make_pair(std::shared_ptr<qevercloud_generator::Parser::ConstValue>(B), std::shared_ptr<qevercloud_generator::Parser::ConstValue>(C));
  A->prepend(p);
}
constmapvaluelist(A) ::= constvalue(B) COLON constvalue(C) LISTSEP constmapvaluelist(D).
{
  A = D;
  auto p = std::make_pair(std::shared_ptr<qevercloud_generator::Parser::ConstValue>(B), std::shared_ptr<qevercloud_generator::Parser::ConstValue>(C));
  A->prepend(p);
}

