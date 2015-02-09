// LLVMIRCompiler.cpp

#include "precompile.h"
#include "LLVMIRCompiler.h"

using namespace llvm;

// General options for llc.  Other pass-specific options are specified
// within the corresponding llc passes, and target-specific options
// and back-end code generation options are specified with the target machine.
//
static cl::opt<std::string>
  InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string>
  OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

static cl::opt<unsigned>
  TimeCompilations("time-compilations", cl::Hidden, cl::init(1u),
  cl::value_desc("N"),
  cl::desc("Repeat compilation N times for timing"));

static cl::opt<bool>
  NoIntegratedAssembler("no-integrated-as", cl::Hidden,
  cl::desc("Disable integrated assembler"));

// Determine optimization level.
static cl::opt<char>
  OptLevel("O",
  cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
  "(default = '-O2')"),
  cl::Prefix,
  cl::ZeroOrMore,
  cl::init(' '));

static cl::opt<std::string>
  TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<bool> NoVerify("disable-verify", cl::Hidden,
                              cl::desc("Do not verify input module"));

static cl::opt<bool> DisableSimplifyLibCalls("disable-simplify-libcalls",
                                             cl::desc("Disable simplify-libcalls"));

static cl::opt<bool> ShowMCEncoding("show-mc-encoding", cl::Hidden,
                                    cl::desc("Show encoding in .s output"));

static cl::opt<bool> EnableDwarfDirectory(
  "enable-dwarf-directory", cl::Hidden,
  cl::desc("Use .file directives with an explicit directory."));

static cl::opt<bool> AsmVerbose("asm-verbose",
                                cl::desc("Add comments to directives."),
                                cl::init(true));

// GetFileNameRoot - Helper function to get the basename of a filename.
static inline std::string
  GetFileNameRoot(const std::string &InputFilename) {
    std::string IFN = InputFilename;
    std::string outputFilename;
    int Len = IFN.length();
    if ((Len > 2) &&
      IFN[Len-3] == '.' &&
      ((IFN[Len-2] == 'b' && IFN[Len-1] == 'c') ||
      (IFN[Len-2] == 'l' && IFN[Len-1] == 'l'))) {
        outputFilename = std::string(IFN.begin(), IFN.end()-3); // s/.bc/.s/
    } else {
      outputFilename = IFN;
    }
    return outputFilename;
}

#include "llvm/CodeGen/CommandFlags.h"

static tool_output_file *GetOutputStream(const char *TargetName,
                                         Triple::OSType OS,
                                         const char *ProgName) 
{
  // If we don't yet have an output filename, make one.
  if (OutputFilename.empty()) {
    if (InputFilename == "-")
      OutputFilename = "-";
    else {
      OutputFilename = GetFileNameRoot(InputFilename);

      switch (FileType) {
      case TargetMachine::CGFT_AssemblyFile:
        if (TargetName[0] == 'c') {
          if (TargetName[1] == 0)
            OutputFilename += ".cbe.c";
          else if (TargetName[1] == 'p' && TargetName[2] == 'p')
            OutputFilename += ".cpp";
          else
            OutputFilename += ".s";
        } else
          OutputFilename += ".s";
        break;
      case TargetMachine::CGFT_ObjectFile:
        if (OS == Triple::Win32)
          OutputFilename += ".obj";
        else
          OutputFilename += ".o";
        break;
      case TargetMachine::CGFT_Null:
        OutputFilename += ".null";
        break;
      }
    }
  }

  // Decide if we need "binary" output.
  bool Binary = false;
  switch (FileType) {
  case TargetMachine::CGFT_AssemblyFile:
    break;
  case TargetMachine::CGFT_ObjectFile:
  case TargetMachine::CGFT_Null:
    Binary = true;
    break;
  }

  // Open the file.
  std::string error;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
  if (!Binary)
    OpenFlags |= sys::fs::F_Text;
  tool_output_file *FDOut = new tool_output_file(OutputFilename.c_str(), error,
    OpenFlags);
  if (!error.empty()) {
    errs() << error << '\n';
    delete FDOut;
    return nullptr;
  }

  return FDOut;
}

//// main - Entry point for the llc compiler.
////
//int main(int argc, char **argv) {
//	sys::PrintStackTraceOnErrorSignal();
//	PrettyStackTraceProgram X(argc, argv);
//
//	// Enable debug stream buffering.
//	EnableDebugBuffering = true;
//
//	LLVMContext &Context = getGlobalContext();
//	llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
//
//	// Initialize targets first, so that --version shows registered targets.
//	InitializeAllTargets();
//	InitializeAllTargetMCs();
//	InitializeAllAsmPrinters();
//	InitializeAllAsmParsers();
//
//	// Initialize codegen and IR passes used by llc so that the -print-after,
//	// -print-before, and -stop-after options work.
//	PassRegistry *Registry = PassRegistry::getPassRegistry();
//	initializeCore(*Registry);
//	initializeCodeGen(*Registry);
//	initializeLoopStrengthReducePass(*Registry);
//	initializeLowerIntrinsicsPass(*Registry);
//	initializeUnreachableBlockElimPass(*Registry);
//
//	// Register the target printer for --version.
//	cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);
//
//	cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");
//
//	// Compile the module TimeCompilations times to give better compile time
//	// metrics.
//	for (unsigned I = TimeCompilations; I; --I)
//		if (int RetVal = compileModule(argv, Context))
//			return RetVal;
//	return 0;
//}

static int compileModule(char **argv, LLVMContext &Context, Module *mod) {
  // Load the module to be compiled...
  SMDiagnostic Err;
  //std::unique_ptr<Module> M;
  //Module *mod = nullptr;
  Triple TheTriple;

  //bool SkipModule = MCPU == "help" ||
  //	(!MAttrs.empty() && MAttrs.front() == "help");

  // If user asked for the 'native' CPU, autodetect here. If autodection fails,
  // this will set the CPU to an empty string which tells the target to
  // pick a basic default.
  if (MCPU == "native" || MCPU == "")
    MCPU = sys::getHostCPUName();

  //// If user just wants to list available options, skip module loading
  //if (!SkipModule) {
  //	M.reset(ParseIRFile(InputFilename, Err, Context));
  //	mod = M.get();
  //	if (mod == nullptr) {
  //		Err.print(argv[0], errs());
  //		return 1;
  //	}

  // If we are supposed to override the target triple, do so now.
  if (!TargetTriple.empty())
    mod->setTargetTriple(Triple::normalize(TargetTriple));
  TheTriple = Triple(mod->getTargetTriple());
  //} else {
  //	TheTriple = Triple(Triple::normalize(TargetTriple));
  //}

  if (TheTriple.getTriple().empty())
    TheTriple.setTriple(sys::getDefaultTargetTriple());


  // Get the target specific parser.
  std::string Error;
  const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple,
    Error);
  if (!TheTarget) {
    errs() << argv[0] << ": " << Error;
    return 1;
  }

  // Package up features to be passed to target/subtarget
  std::string FeaturesStr;
  if (MAttrs.size()) {
    SubtargetFeatures Features;
    for (unsigned i = 0; i != MAttrs.size(); ++i)
      Features.AddFeature(MAttrs[i]);
    FeaturesStr = Features.getString();
  }

  CodeGenOpt::Level OLvl = CodeGenOpt::None;
  switch (OptLevel) {
  default:
    errs() << argv[0] << ": invalid optimization level.\n";
    return 1;
  case ' ': break;
  case '0': OLvl = CodeGenOpt::None; break;
  case '1': OLvl = CodeGenOpt::Less; break;
  case '2': OLvl = CodeGenOpt::Default; break;
  case '3': OLvl = CodeGenOpt::Aggressive; break;
  }

  TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
  Options.DisableIntegratedAS = NoIntegratedAssembler;
  Options.MCOptions.ShowMCEncoding = ShowMCEncoding;
  Options.MCOptions.MCUseDwarfDirectory = EnableDwarfDirectory;
  Options.MCOptions.AsmVerbose = AsmVerbose;

  std::unique_ptr<TargetMachine> target(
    TheTarget->createTargetMachine(TheTriple.getTriple(), MCPU, FeaturesStr,
    Options, RelocModel, CMModel, OLvl));
  assert(target.get() && "Could not allocate target machine!");

  // If we don't have a module then just exit now. We do this down
  // here since the CPU/Feature help is underneath the target machine
  // creation.
  //if (SkipModule)
  //	return 0;

  assert(mod && "Should have exited if we didn't have a module!");
  TargetMachine &Target = *target.get();

  if (GenerateSoftFloatCalls)
    FloatABIForCalls = FloatABI::Soft;

  // Figure out where we are going to send the output.
  std::unique_ptr<tool_output_file> Out(
    GetOutputStream(TheTarget->getName(), TheTriple.getOS(), argv[0]));
  if (!Out) return 1;

  // Build up all of the passes that we want to do to the module.
  PassManager PM;

  // Add an appropriate TargetLibraryInfo pass for the module's triple.
  TargetLibraryInfo *TLI = new TargetLibraryInfo(TheTriple);
  if (DisableSimplifyLibCalls)
    TLI->disableAllFunctions();
  PM.add(TLI);

  // Add the target data from the target machine, if it exists, or the module.
  if (const DataLayout *DL = Target.getDataLayout())
    mod->setDataLayout(DL);
  PM.add(new DataLayoutPass(mod));

  if (RelaxAll.getNumOccurrences() > 0 &&
    FileType != TargetMachine::CGFT_ObjectFile)
    errs() << argv[0]
  << ": warning: ignoring -mc-relax-all because filetype != obj";

  {
    formatted_raw_ostream FOS(Out->os());

    AnalysisID StartAfterID = nullptr;
    AnalysisID StopAfterID = nullptr;
    const PassRegistry *PR = PassRegistry::getPassRegistry();
    if (!StartAfter.empty()) {
      const PassInfo *PI = PR->getPassInfo(StartAfter);
      if (!PI) {
        errs() << argv[0] << ": start-after pass is not registered.\n";
        return 1;
      }
      StartAfterID = PI->getTypeInfo();
    }
    if (!StopAfter.empty()) {
      const PassInfo *PI = PR->getPassInfo(StopAfter);
      if (!PI) {
        errs() << argv[0] << ": stop-after pass is not registered.\n";
        return 1;
      }
      StopAfterID = PI->getTypeInfo();
    }

    // Ask the target to add backend passes as necessary.
    if (Target.addPassesToEmitFile(PM, FOS, FileType, NoVerify,
      StartAfterID, StopAfterID)) {
        errs() << argv[0] << ": target does not support generation of this"
          << " file type!\n";
        return 1;
    }

    // Before executing passes, print the final values of the LLVM options.
    cl::PrintOptionValues();

    PM.run(*mod);
  }

  // Declare success.
  Out->keep();


  return 0;
}

std::string exec(char* cmd) {
  FILE* pipe = _popen(cmd, "r");
  if (!pipe) return "ERROR";
  char buffer[128];
  std::string result = "";
  while(!feof(pipe)) {
    if(fgets(buffer, 128, pipe) != NULL)
      result += buffer;
  }
  _pclose(pipe);
  return result;
}

void IRCompile(LLVMState &llvm, const char* obj_file, const char* exe_file, const char* extra_link_options)
{
  sys::PrintStackTraceOnErrorSignal();
  //PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  LLVMContext &Context = getGlobalContext();
  //llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Initialize targets first, so that --version shows registered targets.
  //InitializeAllTargets();
  LLVMInitializeNativeTarget();
  //InitializeAllTargetMCs();
  LLVMInitializeX86TargetMC();
  //InitializeAllAsmPrinters();
  LLVMInitializeNativeAsmPrinter();
  //InitializeAllAsmParsers();
  LLVMInitializeNativeAsmParser();

  // Initialize codegen and IR passes used by llc so that the -print-after,
  // -print-before, and -stop-after options work.
  PassRegistry *Registry = PassRegistry::getPassRegistry();
  initializeCore(*Registry);
  initializeCodeGen(*Registry);
  initializeLoopStrengthReducePass(*Registry);
  initializeLowerIntrinsicsPass(*Registry);
  initializeUnreachableBlockElimPass(*Registry);

  // Register the target printer for --version.
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  //cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");
  //StartAfter = 

  FileType = TargetMachine::CGFT_ObjectFile;
  OutputFilename = obj_file;

  const char *name = "gg.exe";
  compileModule((char **)&name, *llvm.context, llvm.module);

  //system("link me.obj /ENTRY:main");
  std::string link_command = std::string("..\\mylink.bat ") + std::string(obj_file) + std::string(extra_link_options) + std::string(" /OUT:") + std::string(exe_file);
  system(link_command.c_str());

  //exec("vcvars32.bat");
  //exec("link me.obj /ENTRY:main");
  //exec("vcvars32.bat");
}

