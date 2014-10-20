// gg_identifier_db.cpp

#include "precompile.h"
#include "GGCompiler.h"

enum GGIdentifierType {
  IDENTIFIER_FUNCTION,
  IDENTIFIER_VARIABLE,
  IDENTIFIER_DATATYPE,

  IDENTIFIER_DATATYPE_TYPE,  // { struct, enum, typeset, typedef, llvm_type }
};

enum GGIdentifierScope {
  SCOPE_GLOBAL,
  SCOPE_LOCAL,
};

typedef unsigned long uint32;

struct GGIdentifier	 {
  uint32 name_hash;
  GGSubString name;
  GGIdentifierType identifier_type;
  GGIdentifierScope identifier_scope;
  GGIdentifier *data_type;
  GGIdentifier *function_params;
  int num_function_params;
};

const int MAX_IDENTIFIERS = 1024;

struct GGIdentifierDB {
  GGIdentifier identifiers[MAX_IDENTIFIERS];
  int num_identifiers;
};

uint32 crc32(const void *bytes, int lenght) {
  return 0;
}

uint32 substring_hash(const GGSubString &str) {
  return crc32(str.start, str.length);
}

bool param_list_equal(GGIdentifier *param_list, int num_params, GGIdentifier *param_list_2, int num_params_2)
{
  if (num_params != num_params_2) return false;
  for(int i = 0; i < num_params; ++i) {
    if (param_list[i].data_type != param_list_2[i].data_type) return false;
  }

  return true;
}

GGIdentifier *IdentifierDBLookupFunction(GGIdentifierDB &db, const GGSubString &name, GGIdentifier *param_list, int num_params)
{
  uint32 hash_name = substring_hash(name);
  for(int i = 0; i < db.num_identifiers; ++i) {
    GGIdentifier &identifier = db.identifiers[i];
    if (identifier.name_hash == hash_name &&
      name.length == identifier.name.length &&
      strncmp(name.start, identifier.name.start, name.length) == 0) 
    {
      if (identifier.identifier_type != IDENTIFIER_FUNCTION) return &identifier;
      if (param_list_equal(param_list, num_params, identifier.function_params, identifier.num_function_params))

        return &identifier;
    }
  }

  return NULL;
}

GGIdentifier *IdentifierDBLookup(GGIdentifierDB &db, const GGSubString &name)
{
  uint32 hash_name = substring_hash(name);
  for(int i = 0; i < db.num_identifiers; ++i) {
    GGIdentifier &identifier = db.identifiers[i];
    if (identifier.name_hash == hash_name &&
      name.length == identifier.name.length &&
      strncmp(name.start, identifier.name.start, name.length) == 0) 
    {
      return &identifier;
    }
  }

  return NULL;
}

GGIdentifier *IdentifierDBAlloc(GGIdentifierDB &db) {
  assert(db.num_identifiers < MAX_IDENTIFIERS);
  return &db.identifiers[db.num_identifiers++];
}

struct ErrorRetval {
  char error_string[1024];
};

void error(const char *error_string, ...);

bool isDataTypeType(const GGIdentifier *identifier) {
  return identifier->identifier_type == IDENTIFIER_DATATYPE_TYPE;
}

bool isDataType(const GGIdentifier *identifier) {
  return identifier->identifier_type == IDENTIFIER_DATATYPE;
}

void IdentifierDBAddDataTypeType(GGIdentifierDB &db, const char *type) {
  GGIdentifier *identifier = IdentifierDBAlloc(db);
  identifier->identifier_type = IDENTIFIER_DATATYPE_TYPE;
  identifier->name.start = type;
  identifier->name.length = strlen(type);
  identifier->name_hash = substring_hash(identifier->name);
}

void IdentifierDBInit(GGIdentifierDB &db) {
  IdentifierDBAddDataTypeType(db, "struct");
  IdentifierDBAddDataTypeType(db, "enum");
  IdentifierDBAddDataTypeType(db, "llvm_type");
  IdentifierDBAddDataTypeType(db, "typedef");
}

void IdentifierDBAddUnknownType(GGIdentifierDB &db, const GGToken &name) {
  GGIdentifier *identifier = IdentifierDBLookup(db, name.substring);
  assert(identifier == NULL);

  GGIdentifier *newIdentifier = IdentifierDBAlloc(db);
  newIdentifier->name_hash = substring_hash(name.substring);
  newIdentifier->identifier_type = IDENTIFIER_DATATYPE;
  //newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
  newIdentifier->data_type;
}

void IdentifierDBAddType(GGIdentifierDB &db, const GGToken &name, const GGToken &type) {
  GGIdentifier *identifier = IdentifierDBLookup(db, name.substring);
  if (identifier != NULL) {
    return error("duplicate identifier %s, first defined as %s");
  }

  GGIdentifier *type_identifier = IdentifierDBLookup(db, type.substring);
  if (type_identifier == NULL) {
    return error("unknown datatype type for %s");
  }
  else if (!isDataTypeType(type_identifier)) {
    return error("invalid datatype type for %s, first defined as %s");
  }

  GGIdentifier *newIdentifier = IdentifierDBAlloc(db);
  newIdentifier->name_hash = substring_hash(name.substring);
  newIdentifier->identifier_type = IDENTIFIER_DATATYPE;
  //newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
  newIdentifier->data_type = type_identifier;
}

void IdentifierDBAddVariable(GGIdentifierDB &db, const GGToken &name, const GGToken &type) {
  GGIdentifier *identifier = IdentifierDBLookup(db, name.substring);
  if (identifier != NULL) {
    return error("duplicate identifier %s, first defined as %s");
  }

  GGIdentifier *type_identifier = IdentifierDBLookup(db, type.substring);
  if (type_identifier == NULL) {
    IdentifierDBAddUnknownType(db, type);
  } else if (!isDataType(type_identifier)) {
    return error("invalid datatype for %s, first defined as %s");
  }

  GGIdentifier *newIdentifier = IdentifierDBAlloc(db);
  newIdentifier->name_hash = substring_hash(name.substring);
  newIdentifier->identifier_type = IDENTIFIER_VARIABLE;
  //newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
  newIdentifier->data_type = type_identifier;
}

GGIdentifier *ParamListToIdentifiers(GGIdentifierDB &db, const GGToken &param_list) {
  GGIdentifier *retval = db.identifiers+db.num_identifiers;

  for(int i = 0; i < param_list.num_subtokens; ++i) {
    GGIdentifier *param_identifier = IdentifierDBAlloc(db);
    GGToken &param = param_list.subtokens[i];
    assert(param.token == TOKEN_COMPOUND_FUNCTION_PARAM);
    assert(param.num_subtokens == 2);
    GGToken &type = param.subtokens[0];
    GGToken &name = param.subtokens[1];
    IdentifierDBAddVariable(db, name, type);
  }

  return retval;
}

void IdentifierDBAddFunction(GGIdentifierDB &db, const GGToken &name, const GGToken &param_list, const GGToken &retval) {
  GGIdentifier *param_identifiers = ParamListToIdentifiers(db, param_list);
  GGIdentifier *identifier = IdentifierDBLookupFunction(db, name.substring, param_identifiers, param_list.num_subtokens);
  if (identifier != NULL) {
    return error("duplicate identifier %s, first defined as %s");
  }

  GGIdentifier *type_identifier = IdentifierDBLookup(db, retval.substring);
  if (type_identifier == NULL) {
    IdentifierDBAddUnknownType(db, retval);
  } else if (!isDataType(type_identifier)) {
    return error("invalid return type identifier  %s, first defined as %s");
  }

  GGIdentifier *newIdentifier = IdentifierDBAlloc(db);
  newIdentifier->name_hash = substring_hash(name.substring);
  newIdentifier->identifier_type = IDENTIFIER_FUNCTION;
  //newIdentifier->identifier_scope = lexical_scope_depth > 0 ? LOCAL : GLOBAL;
  newIdentifier->function_params = param_identifiers;
  newIdentifier->num_function_params = param_list.num_subtokens;
}

