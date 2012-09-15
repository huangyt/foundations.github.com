// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/JavaInterpreter.cpp,v 1.22 2005/04/20 09:13:54 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include "JavaInterpreter.h"
#include "JavaObject.h"

namespace acdk {
namespace java {

using namespace acdk::lang;
using namespace acdk::io;

#define USER_CLASSPATH "."
#define PATH_SEPARATOR ';' 

//static 
JavaInterpreter* JavaInterpreter::curJInt = 0;

RJavaInterpreter 
getJavaInterpreter()
{
  if (JavaInterpreter::curJInt != 0)
    return JavaInterpreter::curJInt;

  static RJavaInterpreter tjavaint = new JavaInterpreter(); // was created outside
  return JavaInterpreter::curJInt;
}

JavaInterpreter::JavaInterpreter()
: Object(),
  _env(0),
  _jvm(0)
{
#ifdef JNI_VERSION_1_2
  JavaVMInitArgs vm_args;
  JavaVMOption   options[3];

  RString acdkhome = System::getProperty("ACDKHOME");
  if (acdkhome != Nil)
  {
    RString str = "-Djava.class.path=" + acdkhome + ::acdk::io::File::separator() + "bin";
    options[0].optionString = strdup(str->c_str());
  } else {
    options[0].optionString = "";
  }
  
  //options[1].optionString = "-verbose:jni";
  vm_args.nOptions = 1;
  
  vm_args.version = JNI_VERSION_1_2; //JNI_VERSION_1_2; //0x00010002;
  vm_args.options = options;
  vm_args.ignoreUnrecognized = true;

  

/* Create the Java VM */
  JNIEnv* tjenv;
  int res = JNI_CreateJavaVM(&_jvm, (void**)&tjenv, &vm_args);
  _env = tjenv;
#else
    JDK1_1InitArgs vm_args;
    char classpath[1024];
    vm_args.version = 0x00010001;
    JNI_GetDefaultJavaVMInitArgs(&vm_args);
    /* Append USER_CLASSPATH to the default system class path */
    sprintf(classpath, "%s%c%s",
            vm_args.classpath, PATH_SEPARATOR, USER_CLASSPATH);
    vm_args.classpath = classpath;
/* Create the Java VM */
    int res = JNI_CreateJavaVM(&_jvm, (void**)&_env, &vm_args);
#endif
   if (res < 0) 
    THROW1(Exception, "Can't create Java VM");
   JavaInterpreter::curJInt = this;
}

//virtual 
JavaInterpreter::~JavaInterpreter()
{
  if (_jvm)
  {
    // this call blocks forever! 
    // _jvm->DestroyJavaVM();
  }
  _jvm = 0;
  JavaInterpreter::curJInt = 0;
}


RJavaObject 
JavaInterpreter::getInstance(IN(RString) classname, ScriptVarArray& args)
{
  return JavaObject::getInstance(this, classname, args);
}

void 
JavaInterpreter::attachCurrentThread()
{
#if defined(ACDK_OS_WIN32)
  _jvm->AttachCurrentThread((void**)&_env.get(), 0);
#endif
}

/*
jclass 
JavaInterpreter::getClassOfString()
{
  static jclass __classofstring = 0;
  if (__classofstring == 0) {
    jstring str = jenv()->NewStringUTF("");
    __classofstring = jenv()->GetObjectClass(str);

  }
  return __classofstring;
}
*/

#if 0
JNIEXPORT jint JNICALL
JNI_CreateJavaVM(JavaVM **vm, void **penv, void *args)
{
  ExecEnv *ee;
  
  /* 
  * At the moment it's only possible to have one Java VM, 
  * since some of the runtime state is in global variables. 
  */
  if (VM_created)
    return JNI_EEXIST;
  
  if (InitializeJavaVM(args) == SYS_OK) {
    ExecEnv *ee = EE();
    *vm = (JavaVM *)(&main_vm);
    *(JNIEnv**)penv = EE2JNIEnv(ee);
    
#ifdef HPROF
    if (JVMPI_EVENT_IS_ENABLED(JVMPI_EVENT_THREAD_START))
      jvmpi_thread_start(ee->thread);
#endif /* HPROF */
    
#ifdef BREAKPTS
    if (debugging) 
      notify_debugger_of_thread_start(ee, ee->thread);
#endif /* BREAKPTS */
    
    ee->stack_base = 0;
    
    VM_created = 1;
    return JNI_OK;
  } else {
    *vm = 0;
    *(JNIEnv**)penv = 0;
    return JNI_ERR;
  }
}


int InitializeJavaVM(void *args)
{
  ExecEnv *ee;
  sys_thread_t *sys_tid;
  HThread *self;
  jint version = *(jint*)args;
  jint result;
  props_md_t *sprops;
  
  /* Check if the host application provides an HPI implementation */
  if (version == JNI_VERSION_1_2) {
    JavaVMInitArgs *vm_args = args;
    int i;
    for (i = 0; i < vm_args->nOptions; i++) {
      JavaVMOption *option = vm_args->options + i;
      if (strcmp(option->optionString, "_hpi") == 0) {
        GetHPI = (GetInterfaceFunc)option->extraInfo;
      }
    }
  }
  
  if (GetHPI == NULL) {
    GetHPI = (GetInterfaceFunc)InitializeHPI(&callbacks);
  }
  
  if (GetHPI == NULL ||
    GetHPI((void **)&hpi_memory_interface, "Memory", 1) ||
    GetHPI((void **)&hpi_library_interface, "Library", 1) ||
    GetHPI((void **)&hpi_system_interface, "System", 1) ||
    GetHPI((void **)&hpi_thread_interface, "Thread", 1) ||
    GetHPI((void **)&hpi_file_interface, "File", 1)) {
    return JNI_ERR;
  }
  
  sprops = GetPropertiesMD();
  if (sprops == NULL) {
    jio_fprintf(stderr, "Can't obtain system-specific information\n");
    return JNI_ERR;
  }
  java_home_dir = sprops->java_home;
  java_dll_dir = sprops->dll_dir;
  init_sysclasspath = sprops->sysclasspath;
  
#if defined(DEBUG) && defined(CHECK_JNI)
  jni_NativeInterfacePtr = &checked_jni_NativeInterface;
#else
  jni_NativeInterfacePtr = &unchecked_jni_NativeInterface;
#endif /* DEBUG && CHECK_JNI */
  
  if (version == JNI_VERSION_1_1) {
    result = Initialize11((JDK1_1InitArgs *)args);
  } else if (version == JNI_VERSION_1_2) {
    result = Initialize12((JavaVMInitArgs *)args, TRUE);
  } else {
    jio_fprintf(stderr, "Unrecognized JNI version: 0x%lx\n", version);
    result = JNI_EVERSION;
  }
#ifdef BREAKPTS
  if (debugging) {
    jni_NativeInterfacePtr = jvmdi_jni_GetNativeInterface();
  }
#endif /* BREAKPTS */
  
  if (result < 0)
    return result;
  
#ifndef NO_HIDDEN_ENV
  {
    char *cmd_env = getenv("_JAVA_OPTIONS");
    if (cmd_env && *cmd_env) {
#define N_MAX_OPTIONS 32
      JavaVMInitArgs vm_args;
      JavaVMOption options[N_MAX_OPTIONS];
      int i;
      cmd_env = sysStrdup(cmd_env);
      if (cmd_env == NULL) {
        return JNI_ERR;
      }
      jio_fprintf(stderr, "Picked up _JAVA_OPTIONS: %s\n", cmd_env);
      for (i = 0; i < N_MAX_OPTIONS;) {
        options[i++].optionString = cmd_env;
        if (*cmd_env != '-') {
          jio_fprintf(stderr, "Bad _JAVA_OPTIONS: %s\n", cmd_env);
          return JNI_ERR;
        }
        while (*cmd_env && *cmd_env != ' ') {
          cmd_env++;
        }
        if (*cmd_env == 0) {
          break;
        }
        *cmd_env++ = 0;
        while (*cmd_env == ' ') {
          cmd_env++;
        }
      }
      vm_args.version = JNI_VERSION_1_2;
      vm_args.options = options;
      vm_args.nOptions = i;
      vm_args.ignoreUnrecognized = FALSE;
      if (Initialize12(&vm_args, FALSE) < 0) {
        return JNI_ERR;
      }
    }
  }
  
  {
    char *sizestr = getenv("_MIN_JAVASTACK_CHUNK_SIZE");
    if (sizestr) {
      min_javastack_chunk_size = atoi(sizestr);
      jio_fprintf(stderr, "Java stack chunks set to %d bytes.\n",
        min_javastack_chunk_size);
    }
  }
#endif /* NO_HIDDEN_ENV */
  
  /* Set the rest of callbacks slots to appropriate values */
#ifdef LOGGING
  sysSetLoggingLevel(logging_level);
#endif
  
  /* Initialize the monitor registry */
  monitorRegistryInit();
  
  if (sysThreadBootstrap(&sys_tid, &_queue_lock,
    offsetof(ExecEnv, sys_thr))) {
    jio_fprintf(stderr, "Can't bootstrap threads\n");
    return JNI_ERR;
  }
  
  ee = SysThread2EE(sys_tid);
  
  /* The setting of the stack base pointer is only temporarly.
  * JNI_CreateJavaVM will later set stack_base back to zero.
  * (Invocation API functions are resilient to stack location.)
  */
  if (!InitializeExecEnv(ee, 0, &args)) {
    return JNI_ENOMEM;
  }
  
  AdjustUserThreadCount(1);
  
  /* Create the monitor cache */
  monitorCacheInit();
  
#ifdef DEBUG
  pExecuteJava = ExecuteJava_C;
#else
  if (debugging) {
    pExecuteJava = ExecuteJava_C;
  } else {
    pExecuteJava = ExecuteJava;
  }
#endif /* DEBUG */
  
  if (dll_name) {
    if (!loadJVMHelperLib(dll_name, dll_options)) {
      return JNI_ERR;
    }
    
    sysFree(dll_name);
    sysFree(dll_options);
  }
  
  if (initHeapSize < MIN_HEAP_SIZE) {
    jio_fprintf(stderr, "The specified initial heap size is too small. "
		    "(%d bytes required.)\n", MIN_HEAP_SIZE);
    return JNI_EINVAL;
  }
  if (initHeapSize > maxHeapSize) {
    jio_fprintf(stderr,
      "Incompatible initial and maximum heap sizes specified:\n\n");
    jio_fprintf(stderr,
      "    initial size: %d bytes, maximum size: %d bytes\n\n",
      initHeapSize, maxHeapSize);
    jio_fprintf(stderr,
      "The initial heap size must be less than or equal to the maximum heap size.\n");
    jio_fprintf(stderr,
      "The default initial and maximum heap sizes are %d and %d bytes.\n",
      MINHEAPSIZE, MAXHEAPSIZE);
    return JNI_EINVAL;
  }
  
  if (InitializeAlloc(maxHeapSize, initHeapSize) != TRUE) {
    jio_fprintf(stderr, "Unable to initialize Java heap.\n");
    return JNI_ENOMEM;
  }
  
  UseLosslessQuickOpcodes = TRUE; 
  if (!InitializeInterpreter()) {
    return JNI_ENOMEM;
  }
  
  /* Check cache parameters */
  if (allocLocalSize >= allocCacheSize) {
    jio_fprintf(stderr,
      "Allocation cache size (%d) must be greater than "
      "local allocation size (%d)\n",
      allocCacheSize, allocLocalSize);
    jio_fprintf(stderr,
      "The default cache and local allocation sizes are "
      " %d bytes and %d bytes\n",
      ALLOC_CACHE_SIZE, ALLOC_LOCAL_SIZE);
    jio_fprintf(stderr,
      "Note: You can turn off local allocation via '-Xml0'\n.");
    return JNI_EINVAL;
  }
  
  if (!LoadJavaLibrary() || !LoadZipLibrary()) {
    return JNI_ERR;
  }
  
  /* Before this point we must not load any classes. Class loading
  * requires the Java and Zip libraries to be loaded.
  */
  
  /* Preallocate the internal exception objects */
  exceptionInit();
  
  self = InitializeClassThread(ee);
  if (self == 0) {
    if (exceptionOccurred(ee)) {
      goto error;
    }
    jio_fprintf(stderr, "Unable to initialize threads\n");
    return JNI_ERR;
  }
  
  InitializeSignals();
  
  if (TimeSlice) {
    sysAdjustTimeSlice(TimeSlice);
  }
  
  InitializeRefs();
  
  {
    ClassClass *cb = FindClass(ee, "java/lang/System", TRUE);
    if (exceptionOccurred(ee)) {
      goto error;
    }
    execute_java_static_method(ee, cb, "initializeSystemClass", "()V");
    if (exceptionOccurred(ee)) {
      goto error;
    }
  }
  
  if (!InitializeSystemClassLoader()) {
    goto error;
  }
  
  /* A temporary workaround to delay the loading of JIT to reduce
  * startup time. The JIT is loaded when the JNI FindClass function
  * is called.
  */
#ifdef DELAY_JIT_LOADING
#else
  FindClass(ee, JAVAPKG "Compiler", TRUE);
  if (!compilerInitialized) {
    /* Either the compiler couldn't be loaded, or it wasn't initialized */
    UseLosslessQuickOpcodes = FALSE;
  }
#endif
  
#ifdef BREAKPTS
  if (debugging) {
    /* So that field watchpoints can work */
    UseLosslessQuickOpcodes = TRUE;
  }
#endif /* BREAKPTS */
  
  if (debugging && debugPort >= 0) {
  /* JNI FindClass is base class loader aware. Since Agent class
	 * is in tools.jar, the Agent has to be found with JNI FindClass
    * as opposed to the VM's FindClass */
    JNIEnv *env = EE2JNIEnv(ee);
    ClassClass *agentcb = (ClassClass *)DeRef(env,
      (*env)->FindClass(env, "sun/tools/agent/Agent"));
    if (exceptionOccurred(ee)) {
      goto error;
    }
    execute_java_static_method(0, agentcb, "boot", "(I)V", debugPort);
    if (exceptionOccurred(ee)) {
      goto error;
    }
  }
  
#ifndef FAST_MONITOR
  /* Expand the monitor cache if the default was overridden */
  if (specified_monitor_cache_size - monCount > 0) {
    monitorCacheExpand(specified_monitor_cache_size - monCount);
  }
#endif/*FAST_MONITOR*/
  
#ifdef HPROF
  if (jvmpi_event_flags) {
    if (jvmpi_jvm_init_done() < 0) {
      jio_fprintf(stderr, "profiler error\n");
      return JNI_ERR;
    }
  }
#endif /* HPROF */
  
#ifdef BREAKPTS
  if (debugging) {
    notify_debugger_of_vm_init(ee);
  }
#endif /* BREAKPTS */
  
  return JNI_OK;
  
error:
  exceptionDescribe(ee);
  return JNI_ERR;
}

#endif //0
/*
void 
JavaInterpreter::parse(RStringArray args)
{
  jclass cls = _env->FindClass("Prog");
  jmethodID mid = _env->GetStaticMethodID(cls, "main", "([Ljava/lang/String;)V");
}
 
int 
JavaInterpreter::run()
{
  return 0;
}

int 
JavaInterpreter::interactive(RReader in, RWriter out, RWriter err)
{
  return 0;
}

void 
JavaInterpreter::eval(RString code)
{
}
 */

void
JavaInterpreter::callMain(IN(RString) clazz, IN(RStringArray) args)
{
  jclass cls = jenv()->FindClass(clazz->c_str());
  if (cls == 0) {
    System::err->println("Cannot find java class: " + clazz);
    return ;
  }
  jmethodID mid = jenv()->GetStaticMethodID(cls, "main", "([Ljava/lang/String;)V");
  if (mid == 0) {
    System::err->println("Cannot find method main in class " + clazz);
    return ;
  }
  jclass stringClass = jenv()->FindClass("java/lang/String");
  jobjectArray jargs = jenv()->NewObjectArray(args->length(), stringClass, 0);
  for (int i = 0; i < args->length(); i++) 
  {
    jstring jstr = jenv()->NewStringUTF(args[i]->c_str());
    jenv()->SetObjectArrayElement(jargs, i, jstr);
  }
  jenv()->CallStaticVoidMethod(cls, mid, jargs);
}
/*

acdk::lang::dmi::ScriptVar 
JavaInterpreter::call(RString clazz, RString func, acdk::lang::dmi::ScriptVarArray& args)
{
  acdk::lang::dmi::ScriptVar s;
  jclass cls = _env->FindClass(clazz->c_str());
  if (cls == 0) {
    System::err->println("Cannot find java class: " + clazz);
    return s;
  }
  jmethodID mid = _env->GetStaticMethodID(cls, "main", "([Ljava/lang/String;)V");
  if (mid == 0) {
    System::err->println("Cannot find method main in class " + clazz);
    return s;
  }
  
  jstring jstr = _env->NewStringUTF(" from ACDK!");

  jclass stringClass = _env->FindClass("java/lang/String");
  jobjectArray jargs = _env->NewObjectArray(1, stringClass, jstr);
  _env->CallStaticVoidMethod(cls, mid, jargs);
  return s;
}
*/
  

  
} //namespace java
} // namespace acdk


