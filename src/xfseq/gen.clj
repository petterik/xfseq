(ns xfseq.gen
  (:require
    [clojure.string :as string]
    [xfseq.protocols :as p]
    [xfseq.analyze :as ana])
  (:import
    [clojure.asm ClassWriter FieldVisitor MethodVisitor AnnotationVisitor Opcodes Type Label]))

(set! *warn-on-reflection* true)

(defn- invoke-interface [^MethodVisitor mv ^Class class ^String method ^String type-sig]
  (.visitMethodInsn mv
    Opcodes/INVOKEINTERFACE
    (Type/getInternalName class)
    method
    type-sig
    true))

(defn aload-local [^MethodVisitor mv idx]
  (.visitVarInsn mv Opcodes/ALOAD idx))

(defn iload-local [^MethodVisitor mv idx]
  (.visitVarInsn mv Opcodes/ILOAD idx))

(def type->letter (comp #(Character/toUpperCase ^char %) first str))

(defn generate-xfseq-simple [xf-arg-sym input-sym check-reduced? chunk-mode]
  (let [class-name (str "xfseq.gen.XFSeqStep_"
                     (type->letter xf-arg-sym)
                     (type->letter input-sym)
                     (type->letter check-reduced?)
                     (type->letter (name chunk-mode)))
        iname (.replaceAll class-name "\\." "/")

        buffer-class xfseq.buffer.IXFSeqBuffer
        xf-class (condp = xf-arg-sym
                   'long clojure.lang.IFn$OLO
                   'double clojure.lang.IFn$ODO
                   clojure.lang.IFn)
        seq-class clojure.lang.ISeq

        arity-1-ret-class java.lang.Object
        arity-1-arg-0-class java.lang.Object

        arity-2-ret-class java.lang.Object
        arity-2-arg-0-class java.lang.Object
        arity-2-arg-1-class (condp = xf-arg-sym
                              'long Long/TYPE
                              'double Double/TYPE
                              java.lang.Object)

        chunk-class (condp = input-sym
                      'long xfseq.ILongChunk
                      'double xfseq.IDoubleChunk
                      clojure.lang.IChunk)

        input-class (condp = input-sym
                      'long Long/TYPE
                      'double Double/TYPE
                      java.lang.Object)

        input-type (Type/getDescriptor input-class)
        input-seq-class (condp = input-sym
                          'long xfseq.ILongSeq
                          'double xfseq.IDoubleSeq
                          clojure.lang.ISeq)

        invoke-method (if (= clojure.lang.IFn xf-class)
                        "invoke"
                        "invokePrim")

        cw (ClassWriter. 0)

        buffer-type (Type/getDescriptor buffer-class)
        xf-type (Type/getDescriptor xf-class)
        seq-type (Type/getDescriptor seq-class)

        iseq-type (Type/getDescriptor clojure.lang.ISeq)
        obj-type (Type/getDescriptor java.lang.Object)
        ichunk-type (Type/getDescriptor clojure.lang.IChunk)

        arity-1-ret-type (Type/getDescriptor arity-1-ret-class)
        arity-1-arg-0-type (Type/getDescriptor arity-1-arg-0-class)

        arity-2-ret-type (Type/getDescriptor arity-2-ret-class)
        arity-2-arg-0-type (Type/getDescriptor arity-2-arg-0-class)
        arity-2-arg-1-type (Type/getDescriptor arity-2-arg-1-class)

        invoke-xf (fn [^MethodVisitor mv]
                    ;; Cast input from the seq before it's passed to the xf if the types differ.
                    (when (not= xf-arg-sym input-sym)
                      (if (= invoke-method "invokePrim")
                        (if (= 'Object input-sym)
                          (let [ref-type (case xf-arg-sym
                                           (long double) (Type/getInternalName java.lang.Number))
                                cast-method (condp = xf-arg-sym
                                              'long "longValue"
                                              'double "doubleValue")]
                            (.visitTypeInsn mv Opcodes/CHECKCAST ref-type)
                            (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL
                              ref-type
                              cast-method
                              (format "()%s" arity-2-arg-1-type)
                              false))
                          (.visitInsn mv (condp = input-sym
                                           'long Opcodes/L2D
                                           'double Opcodes/D2L)))
                        ;; invoke-method = invoke
                        ;; Casting the input primitive type to its Object type before calling invoke
                        (let [input-ref-class (condp = input-sym
                                                'long java.lang.Long
                                                'double java.lang.Double)]
                          (.visitMethodInsn mv Opcodes/INVOKESTATIC
                            (Type/getInternalName input-ref-class)
                            "valueOf"
                            (format "(%s)%s" input-type (Type/getDescriptor input-ref-class))
                            false))
                        ))
                    ;; Invoke xf with either invoke or invokePrim
                    (invoke-interface mv xf-class invoke-method
                      (format "(%s%s)%s"
                        arity-2-arg-0-type
                        arity-2-arg-1-type
                        arity-2-ret-type)))]

    ;; Class definition
    (.visit cw
      Opcodes/V1_8                                          ;; version
      (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)              ;; access
      iname                                            ;; class-name
      nil                                                   ;; generics
      "clojure/lang/AFn"                                    ;; super class
      nil)                                                  ;; interfaces

    ;; Visit the inner class of IFn if we're invoking it later.
    (when (not= clojure.lang.IFn xf-class)
      (.visitInnerClass cw
        (Type/getInternalName xf-class)
        (Type/getInternalName clojure.lang.IFn)
        (let [cname (.getName xf-class)
              idx (string/last-index-of cname \$)]
          ;; "OLO" or "ODO"
          (subs cname (inc idx) (.length cname)))
        (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)))

    ;; Fields
    (doto (.visitField cw (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "buf" buffer-type nil nil)
      (.visitEnd))
    (doto (.visitField cw (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "xf" xf-type nil nil)
      (.visitEnd))
    (doto (.visitField cw Opcodes/ACC_PRIVATE "s" seq-type nil nil)
      (.visitEnd))

    ;; Constructor
    (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>"
            (format "(%s%s%s)V" buffer-type xf-type seq-type) nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)                       ;; this
      (.visitMethodInsn Opcodes/INVOKESPECIAL (Type/getInternalName clojure.lang.AFn) "<init>" "()V" false)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 1)
      (.visitFieldInsn Opcodes/PUTFIELD iname "buf" buffer-type)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 2)
      (.visitFieldInsn Opcodes/PUTFIELD iname "xf" xf-type)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 3)
      (.visitFieldInsn Opcodes/PUTFIELD iname "s" seq-type)
      (.visitInsn Opcodes/RETURN)
      ;; TODO: Figure out if it's going to be too hard to calculate these frame sizes.
      (.visitMaxs 2 4)
      (.visitEnd))

    ;; Invoke method
    (let [labels (vec (repeatedly 11 #(Label.)))
            label #(nth labels %)]
        (doto (.visitMethod cw Opcodes/ACC_PUBLIC "invoke" (format "()%s" obj-type), nil, nil)
          (.visitCode)
          ;;;;;
          ;; Store buf as a local variable in idx 1
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitFieldInsn Opcodes/GETFIELD iname "buf" buffer-type)
          (.visitVarInsn Opcodes/ASTORE 1)
          ;;;;
          ;; Start for-loop
          ;; Get ISeq s, and assign it to ISeq c at index 2
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitFieldInsn Opcodes/GETFIELD iname "s" seq-type)
          (invoke-interface clojure.lang.ISeq "seq" (format "()%s" iseq-type))
          (.visitVarInsn Opcodes/ASTORE 2)

          ;; Label for restarting for-loop
          (.visitLabel (label 0))
          (.visitFrame Opcodes/F_APPEND
            2 (into-array Object [(Type/getInternalName buffer-class)
                                  (Type/getInternalName seq-class)])
            0 nil)
          ;; Jump to label 1 if ISeq c is nil (end for-loop)
          (.visitVarInsn Opcodes/ALOAD 2)
          (.visitJumpInsn Opcodes/IFNULL (label 1))
          (cond->
            (= ::mixed chunk-mode)
            (doto
              ;; Check chunked seq
              (.visitVarInsn Opcodes/ALOAD 2)
              (.visitTypeInsn Opcodes/INSTANCEOF (Type/getInternalName clojure.lang.IChunkedSeq))
              ;; Jump to label 2 if ISeq c is not IChunkedSeq
              (.visitJumpInsn Opcodes/IFEQ (label 2))))
          (cond->
            (or (= ::chunked chunk-mode) (= ::mixed chunk-mode))
            ;; Chunked seq processing
            (doto
              (.visitVarInsn Opcodes/ALOAD 2)
              #_(.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName clojure.lang.IChunkedSeq))
              ;; Store IChunk ch at index 3
              (invoke-interface clojure.lang.IChunkedSeq "chunkedFirst" (format "()%s" ichunk-type))
              ;; Cast the chunk to the appropriate input type chunk
              #_(cond->
                  (not= input-sym 'Object)
                  (.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName chunk-class)))

              (.visitVarInsn Opcodes/ASTORE 3)
              (.visitInsn Opcodes/ICONST_0)
              ;; Store i = 0 at index 4
              (.visitVarInsn Opcodes/ISTORE 4)
              (.visitVarInsn Opcodes/ALOAD 0)
              (.visitFieldInsn Opcodes/GETFIELD iname "xf" xf-type)
              (.visitVarInsn Opcodes/ASTORE 5)

              ;; Start for-loop for chunked seq
              (.visitLabel (label 3))
              (.visitFrame Opcodes/F_APPEND
                3 (into-array Object [(Type/getInternalName chunk-class) Opcodes/INTEGER (Type/getInternalName xf-class)])
                0 nil)
              (.visitVarInsn Opcodes/ILOAD 4)
              (.visitVarInsn Opcodes/ALOAD 3)
              (invoke-interface chunk-class "count" "()I")
              ;; Jump to label 4 if count is greater or equal to i
              (.visitJumpInsn Opcodes/IF_ICMPGE (label 4))
              ;; Load and call xf
              (cond->
                check-reduced?
                ;; loading buf so we can compare it to the return of invoking xf.
                (.visitVarInsn Opcodes/ALOAD 1))
              (.visitVarInsn Opcodes/ALOAD 5)
              (.visitVarInsn Opcodes/ALOAD 1)
              (.visitVarInsn Opcodes/ALOAD 3)
              (.visitVarInsn Opcodes/ILOAD 4)
              ;; Invoke nth, possibly primitive.
              (invoke-interface chunk-class
                (condp = input-sym
                  'long "nthLong"
                  'double "nthDouble"
                  "nth")
                (format "(I)%s" input-type))

              (invoke-xf)
              ;; Continue chunked for-loop if buf == return from xf
              (as-> mv
                (if check-reduced?
                  (doto mv
                    (.visitJumpInsn Opcodes/IF_ACMPEQ (label 5)) ;; TODO: Can we use IF_ACMPNE for the GOTO and skip label 5?
                    ;; Jump to label 1 if buf != return (end for-loop)
                    (.visitJumpInsn Opcodes/GOTO (label 1))
                    ;; Continuing chunked for-loop
                    (.visitLabel (label 5))
                    (.visitFrame Opcodes/F_SAME 0 nil 0 nil))
                  ;; When not checking reduced, just pop the return value from xf from the stack.
                  (.visitInsn mv Opcodes/POP)))
              (.visitIincInsn 4 1)
              (.visitJumpInsn Opcodes/GOTO (label 3))
              ;; Ending the chunked block by assigning c = ch.chunkedMore()
              (.visitLabel (label 4))
              (.visitFrame Opcodes/F_CHOP 2 nil 0 nil)
              (.visitVarInsn Opcodes/ALOAD 2)
              #_(.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName clojure.lang.IChunkedSeq))
              (invoke-interface clojure.lang.IChunkedSeq "chunkedMore" (format "()%s" iseq-type))
              (.visitVarInsn Opcodes/ASTORE 2)
              ;; Jump to checking buffer + return at label 6
              (.visitJumpInsn Opcodes/GOTO (label 6))))

          ;; Handling the non-chunked case
          (cond->
            (= ::mixed chunk-mode)
            (doto
              (.visitLabel (label 2))
              (.visitFrame Opcodes/F_CHOP 1 nil 0 nil)))
          (cond->
            (or (= ::dechunked chunk-mode) (= ::mixed chunk-mode))
            (doto
              ;; Load and call xf
              (cond->
                check-reduced?
                ;; Loading buf to compare with invoke-xf return
                (.visitVarInsn Opcodes/ALOAD 1))
              (.visitVarInsn Opcodes/ALOAD 0)
              (.visitFieldInsn Opcodes/GETFIELD iname "xf" xf-type)
              (.visitVarInsn Opcodes/ALOAD 1)
              (.visitVarInsn Opcodes/ALOAD 2)
              #_(cond->
                  (not= 'Object input-sym)
                  (.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName input-seq-class)))
              (invoke-interface input-seq-class
                (condp = input-sym
                  'long "firstLong"
                  'double "firstDouble"
                  "first")
                (format "()%s" input-type))
              (invoke-xf)
              (as-> mv
                (if check-reduced?
                  (doto mv
                    (.visitJumpInsn Opcodes/IF_ACMPEQ (label 7)) ;; TODO: Can we use IF_ACMPNE for the GOTO and skip label 7?
                    (.visitJumpInsn Opcodes/GOTO (label 1))
                    (.visitLabel (label 7))
                    (.visitFrame Opcodes/F_SAME 0 nil 0 nil))
                  ;; When not checking reduced, just pop the return value from xf from the stack.
                  (.visitInsn mv Opcodes/POP)))
              ;; Load and assign ISeq c = c.more()
              (.visitVarInsn Opcodes/ALOAD 2)
              (invoke-interface clojure.lang.ISeq "more" (format "()%s" iseq-type))
              (.visitVarInsn Opcodes/ASTORE 2)))

          ;; Start end of for-loop, checking buffer and returning
          (.visitLabel (label 6))
          (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
          (.visitVarInsn Opcodes/ALOAD 1)
          (invoke-interface buffer-class "isEmpty" "()Z")
          ;; If empty, jump to 8 where the next part of the outer for-loop is setup (c = c.seq)
          (.visitJumpInsn Opcodes/IFNE (label 8))
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitVarInsn Opcodes/ALOAD 2)
          (.visitFieldInsn Opcodes/PUTFIELD iname "s" seq-type)
          ;; Construct the next LazySeq and return
          (.visitVarInsn Opcodes/ALOAD 1)
          (.visitTypeInsn Opcodes/NEW (Type/getInternalName clojure.lang.LazySeq))
          (.visitInsn Opcodes/DUP)
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitMethodInsn Opcodes/INVOKESPECIAL
            (Type/getInternalName clojure.lang.LazySeq)
            "<init>"
            (format "(%s)V" (Type/getDescriptor clojure.lang.IFn))
            false)
          (invoke-interface buffer-class "toSeq" (format "(%s)%s" iseq-type iseq-type))
          (.visitInsn Opcodes/ARETURN)

          ;; Restart for-loop
          (.visitLabel (label 8))
          (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
          (.visitVarInsn Opcodes/ALOAD 2)
          (invoke-interface clojure.lang.ISeq "seq" (format "()%s" iseq-type))
          (.visitVarInsn Opcodes/ASTORE 2)
          (.visitJumpInsn Opcodes/GOTO (label 0))

          ;; Outside for-loop, calling xf.invoke() and returning
          (.visitLabel (label 1))
          (.visitFrame Opcodes/F_CHOP 1 nil 0 nil)
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitFieldInsn Opcodes/GETFIELD iname "xf" xf-type)
          (.visitVarInsn Opcodes/ALOAD 1)
          #_(cond->
            ;; Invoking regular IFn at the end and needs(?) a castcheck to that type.
            (not= clojure.lang.IFn xf-class)
            (.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName clojure.lang.IFn)))
          (invoke-interface clojure.lang.IFn "invoke" (format "(%s)%s" arity-1-ret-type arity-1-arg-0-type))
          (.visitInsn Opcodes/POP)
          (.visitVarInsn Opcodes/ALOAD 1)
          (invoke-interface buffer-class "isEmpty" "()Z")
          ;; if not empty, jump to label 9 where we construct the last value and return.
          (.visitJumpInsn Opcodes/IFEQ (label 9))
          ;; If empty, load null and jump to return below
          (.visitInsn Opcodes/ACONST_NULL)
          (.visitJumpInsn Opcodes/GOTO (label 10))
          (.visitLabel (label 9))
          (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
          (.visitVarInsn Opcodes/ALOAD 1)
          (invoke-interface buffer-class "toTail" (format "()%s" iseq-type))
          (.visitLabel (label 10))
          (.visitFrame Opcodes/F_SAME1 0 nil 1 (into-array Object [(Type/getInternalName java.lang.Object)]))
          (.visitInsn Opcodes/ARETURN)
          (.visitMaxs
            (cond-> 5 (not check-reduced?) dec)
            (cond-> 6 (= ::dechunked chunk-mode) (-> dec dec dec)))
          (.visitEnd)))

    (.visitEnd cw)

    [class-name (.toByteArray cw)]))

(defmacro gen-xf-seq-class [arg-type input-type check-reduced? chunk-mode]
  `(let [[cname# bytes#] (generate-xfseq-simple ~arg-type ~input-type ~check-reduced? ~chunk-mode)
         klass# (.defineClass ^clojure.lang.DynamicClassLoader (deref clojure.lang.Compiler/LOADER)
                  cname#
                  bytes#
                  nil)
         ctor# ^java.lang.reflect.Constructor (aget (.getDeclaredConstructors klass#) 0)]
     (.setAccessible ctor# true)
     ;; TODO: Create instance without using reflection?
     (fn [& args#]
       (.newInstance ctor# (object-array args#)))))

(def xf-seq-ctors
  (into {}
    (map (fn [[arg-type input-type check-reduced? chunk-mode :as args]]
           [args (gen-xf-seq-class arg-type input-type check-reduced? chunk-mode)]))
    (let [types ['Object 'long 'double]]
      (for [a types b types check-reduced? [true false] chunk-mode [::mixed ::chunked ::dechunked]]
        [a b check-reduced? chunk-mode]))))

(defn xf-seq [xf coll]
  (clojure.lang.LazySeq.
    (when-some [s (condp satisfies? coll
                    p/ILongSeqable (p/long-seq coll)
                    p/IDoubleSeqable (p/double-seq coll)
                    (seq coll))]
      (let [buf (case (:xfseq.core/return-hint (meta xf))
                 long (xfseq.buffer.LongBuffer.)
                 double (xfseq.buffer.DoubleBuffer.)
                 Object (xfseq.buffer.ObjectBuffer.)
                 ;; If there's no return hint, use a buffer
                 ;; of the same type as the incoming seq.
                 (condp instance? s
                   xfseq.ILongSeq (xfseq.buffer.LongBuffer.)
                   xfseq.IDoubleSeq (xfseq.buffer.DoubleBuffer.)
                   (xfseq.buffer.ObjectBuffer.)))

            input-type (condp instance? s
                         xfseq.ILongSeq 'long
                         xfseq.IDoubleSeq 'double
                         'Object)

            rf (xf buf)

            ana (ana/analyze-primitive-interfaces (ana/interfaces (class rf)))
            arg-2-type (get-in ana [2 :args 1] 'Object)
            check-reduced? (not (:xfseq.core/no-reduced? (meta xf)))

            chunk-mode (cond
                         (vector? coll) ::chunked
                         (set? coll) ::dechunked
                         (map? coll) ::dechunked
                         (list? coll) ::dechunked
                         (instance? clojure.lang.Range coll) ::chunked
                         (instance? clojure.lang.LongRange coll) ::chunked
                         (instance? clojure.lang.Repeat coll) ::dechunked
                         :else ::mixed)

            xf-seq-ctor (get xf-seq-ctors [arg-2-type input-type check-reduced? chunk-mode])]

       (xf-seq-ctor buf (xf buf) s)))))


(comment

  (xf-seq (map inc) [1 2 3])

  (.analyze (Analyzer. (SimpleVerifier.)) "xfseq/gen/MyOwn" )
  (require '[clojure.java.io :as io])
  (def bytecode
    (let [[cname bytecode] (generate-xfseq-simple 'long 'long false ::chunked)]
      (.defineClass ^clojure.lang.DynamicClassLoader
        (deref clojure.lang.Compiler/LOADER)
        cname
        bytecode
        nil)
      bytecode
      ))

  (import '[xfseq.gen MyOwn])

  (import '[jdk.internal.org.objectweb.asm.tree.analysis Analyzer SimpleVerifier])
  (import '[jdk.internal.org.objectweb.asm.util CheckClassAdapter])
  (import '[jdk.internal.org.objectweb.asm.tree MethodNode])

  (let [v (jdk.internal.org.objectweb.asm.util.TraceClassVisitor. @#'*out*)
        cr (jdk.internal.org.objectweb.asm.ClassReader. bytecode)]
    (.accept cr v 0))

  (import '[clojure.lang AFn])


  (let [cn (jdk.internal.org.objectweb.asm.tree.ClassNode.)
        cr (jdk.internal.org.objectweb.asm.ClassReader. bytecode)]
    (.accept cr (jdk.internal.org.objectweb.asm.util.CheckClassAdapter. cn) 0)

    (doseq [method-obj (.methods cn)]
      (let [^jdk.internal.org.objectweb.asm.tree.MethodNode method method-obj
            a (jdk.internal.org.objectweb.asm.tree.analysis.Analyzer.
                (doto (jdk.internal.org.objectweb.asm.tree.analysis.SimpleVerifier.)
                  (.setClassLoader (deref clojure.lang.Compiler/LOADER))))]
        (prn method)
        (try
          (.analyze a (.name cn) method)
          (catch Throwable e
            (prn (.-desc method))
            e)))))

  (spit "MyOwnClass.class" *1)

  *e
  (gen-interface)
  (require '[clojure.java.io :as io])

  (def real (let [baos (java.io.ByteArrayOutputStream.)]
     (with-open [r (io/reader "classes/production/xfseq/xfseq/XFSeqStepSimple.class")]
       (io/copy r baos))
     (.toByteArray baos)))

  )
