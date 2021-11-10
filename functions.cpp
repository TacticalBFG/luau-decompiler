int getgenv(int L) { // ADD THIS TO YOUR LUA ENVIORNMENT AND CALL IT "getgenv"
  pushvalue(L, LUA_GLOBALSINDEX);
  return 1;
}

bool hasDumpedBytecode = false;

int literallyJustDeserialize(int L, const char *chunk, char *stream, int slen) {
	MH_DisableHook((DWORD*)addys::deserializer);

	int lol = deserialize(L, chunk, stream, slen, 0);
	hasDumpedBytecode = true;

	funcDump = index2adr(L, -1);

	return 1;
}

void pushtval(int L, RTValue *v) {
	RTValue *top = (RTValue*)*(DWORD*)(L + RBX_TOP);
	top->value = v->value;
	top->tt = v->tt;
	RBX_INCR_TOP(L);
}


int getscriptproto(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT "getscriptproto" this will require MinHook  https://github.com/TsudaKageyu/minhook
	void* oldhook = (void*)MH_CreateHook((DWORD*)addys::deserializer, literallyJustDeserialize, NULL);
	MH_CreateHook((DWORD*)addys::deserializer, literallyJustDeserialize, (LPVOID*)oldhook);
	MH_EnableHook((DWORD*)addys::deserializer);

	pushboolean(L, true);
	setfield(L, -2, "Disabled");
	Sleep(2);
	pushboolean(L, false);
	setfield(L, -2, "Disabled");

	while (!hasDumpedBytecode)
		Sleep(1);
	hasDumpedBytecode = false;

	pushtval(L, funcDump);

	return 1;
}

int pushFuncOrStack(int L, int wheresLvl) {
		DWORD ar;
		if (index2adr(L, wheresLvl)->tt == RLUA_TNUMBER) {
			int level = tonumber(L, wheresLvl);
			getinfo(L, level, "f", &ar);
		}
		else {
			pushvalue(L, wheresLvl);
		}

		return 1;
	}

int getconstants(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT  "getconstants"
		if (!pushFuncOrStack(L, -1))
      return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		int sizek = *(DWORD*)(f + proto::sizek);
		RTValue *k = (RTValue*)DEPTR_PROTO(f + proto::k);

		createtable(L, sizek, 0);

		for (int i = 0; i < sizek; i++) {
			pushtval(L, &k[i]);
			rawseti(L, -2, i + 1);
		}

		return 1;
	}

int getprotos(int L) {// ADD THIS TO YOUR LUA ENV AND CALL IT  "getprotos"
		if (!pushFuncOrStack(L, -1))
			return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		int sizep = *(DWORD*)(f + proto::sizep);

		int *p = (int*)DEPTR_PROTO(f + proto::p);

		createtable(L, sizep, 0);

		for (int i = 0; i < sizep; i++) {
			pushproto(L, p[i]);
			rawseti(L, -2, i + 1);
		}

		return 1;
	}

int getinstructions(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT  "getinstructions"
		if (!pushFuncOrStack(L, -1))
			return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		int sizecode = *(DWORD*)(f + proto::sizecode);
		Instruction *code = (Instruction*)DEPTR_PROTO(f + proto::code);

		createtable(L, sizecode, 0);

		for (int i = 0; i < sizecode; i++) {
			Instruction inst = code[i];
			pushnumber(L, inst);
			rawseti(L, -2, i + 1);
		}
		
		return 1;
	}

int getlines(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT  "getlines"
		if (!pushFuncOrStack(L, -1))
			return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		int sizec = *(DWORD*)(f + proto::sizecode);

		unsigned int *lines = (unsigned int*)DEPTR_PROTO(f + proto::lineinfoMajor);
		unsigned char *sexyLines = (unsigned char*)DEPTR_PROTO(f + proto::lineinfoMinor);

		createtable(L, sizec, 0);
		//createtable(L, 0, 0);

		int index = ((sizec - 1) >> 0x18) + 1;

		std::vector<unsigned int> readLines;

		for (int i = 0; i < sizec; i++) {
			int thisLine = sexyLines[i];
			readLines.push_back(thisLine);
		}

		for (int i = 0; i < index; i++) {
			readLines[i] += lines[i];
		}

		for (int i = 0; i < readLines.size(); i++) {
			pushnumber(L, readLines[i]);
			rawseti(L, -2, i + 1);
		}

		
		return 1;
	}
 
int getnups(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT  "getnups"
		if (!pushFuncOrStack(L, -1))
			return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		BYTE nparams = *(BYTE*)(f + proto::nups);

		pushnumber(L, nparams);
		return 1;
	}
 
	int getnparams(int L) { // ADD THIS TO YOUR LUA ENV AND CALL IT  "getnparams"
		if (!pushFuncOrStack(L, -1))
			return 0;

		DWORD v9 = (DWORD)index2adr(L, -1)->value.gc;

		int f = DEPTR_CLOSURE(v9);

		BYTE nparams = *(BYTE*)(f + proto::nparams);

		pushnumber(L, nparams);
		return 1;
	}
