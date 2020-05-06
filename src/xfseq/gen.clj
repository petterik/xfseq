(ns xfseq.gen
  (:import [clojure.asm ClassWriter FieldVisitor MethodVisitor AnnotationVisitor Opcodes Type Label]))

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

(defn generate-xfseq-simple []
  (let [class-name ""                                       ;; fully qualified
        buffer-class xfseq.buffer.IXFSeqBuffer
        seq-class clojure.lang.ISeq
        xf-class clojure.lang.IFn

        arity-1-ret-class java.lang.Object
        arity-1-arg-0-class java.lang.Object

        arity-2-ret-class java.lang.Object
        arity-2-arg-0-class java.lang.Object
        arity-2-arg-1-class java.lang.Object


        cw (ClassWriter. 0)

        buffer-type (Type/getDescriptor buffer-class)
        xf-type (Type/getDescriptor xf-class)
        seq-type (Type/getDescriptor seq-class)

        iseq-type (Type/getDescriptor clojure.lang.ISeq)
        afn-type (Type/getDescriptor clojure.lang.AFn)
        obj-type (Type/getDescriptor java.lang.Object)
        ichunk-type (Type/getDescriptor clojure.lang.IChunk)

        arity-1-ret-type (Type/getDescriptor arity-1-ret-class)
        arity-1-arg-0-type (Type/getDescriptor arity-1-arg-0-class)

        arity-2-ret-type (Type/getDescriptor arity-2-ret-class)
        arity-2-arg-0-type (Type/getDescriptor arity-2-arg-0-class)
        arity-2-arg-1-type (Type/getDescriptor arity-2-arg-1-class)
        ]

    ;; Class definition
    (.visit cw
      Opcodes/V1_8                                          ;; version
      (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)              ;; access
      class-name                                            ;; class-name
      nil                                                   ;; generics
      "clojure/lang/AFn"                                    ;; super class
      nil)                                                  ;; interfaces

    ;; Fields
    (doto (.visitField cw (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "buf" buffer-type nil nil)
      (.visitEnd))
    (doto (.visitField cw (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "xf" xf-type nil nil)
      (.visitEnd))
    (doto (.visitField cw Opcodes/ACC_PRIVATE "s" seq-type nil nil)
      (.visitEnd))

    ;; Constructor
    (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>"
            (format "(%s;%s;%s)" buffer-type xf-type seq-type) nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)                    ;; this
      (.visitMethodInsn Opcodes/INVOKESPECIAL afn-type "<init>" "()V" false)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 1)
      (.visitFieldInsn Opcodes/PUTFIELD class-name "buf" buffer-type)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 2)
      (.visitFieldInsn Opcodes/PUTFIELD class-name "xf" xf-type)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 3)
      (.visitFieldInsn Opcodes/PUTFIELD class-name "s" seq-type)
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
        (.visitFieldInsn Opcodes/GETFIELD class-name "buf" buffer-type)
        (.visitVarInsn Opcodes/ASTORE 1)
        ;;;;
        ;; Start for-loop
        ;; Get ISeq s, and assign it to ISeq c at index 2
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitFieldInsn Opcodes/GETFIELD class-name "s" seq-type)
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
        ;; Check chunked seq
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitTypeInsn Opcodes/INSTANCEOF (Type/getInternalName clojure.lang.IChunkedSeq))
        ;; Jump to label 2 if ISeq c is not IChunkedSeq
        (.visitJumpInsn Opcodes/IFEQ (label 2))
        ;; Chunked seq processing
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName clojure.lang.IChunkedSeq))
        ;; Store IChunk ch at index 3
        (invoke-interface clojure.lang.IChunkedSeq "chunkedFirst" (format "()%s" ichunk-type))
        (.visitVarInsn Opcodes/ASTORE 3)
        (.visitInsn Opcodes/ICONST_0)
        ;; Store i = 0 at index 4
        (.visitVarInsn Opcodes/ISTORE 4)

        ;; Start for-loop for chunked seq
        (.visitLabel (label 3))
        (.visitFrame Opcodes/F_APPEND
          2 (into-array Object [(Type/getInternalName clojure.lang.IChunk) Opcodes/INTEGER])
          0 nil)
        (.visitVarInsn Opcodes/ILOAD 4)
        (.visitVarInsn Opcodes/ALOAD 3)
        (invoke-interface clojure.lang.IChunk "count" "()I")
        ;; Jump to label 4 if count is greater or equal to i
        (.visitJumpInsn Opcodes/IF_ICMPGE (label 4))
        ;; Load and call xf
        (.visitVarInsn Opcodes/ALOAD 1)
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitFieldInsn Opcodes/GETFIELD class-name "xf" xf-type)
        (.visitVarInsn Opcodes/ALOAD 1)
        (.visitVarInsn Opcodes/ALOAD 3)
        (.visitVarInsn Opcodes/ILOAD 4)
        (invoke-interface clojure.lang.IChunk "nth" (format "(I)%s" obj-type)) ;; TODO: Call nthLong/nthDouble depending on input-type
        (invoke-interface xf-class "invoke"                 ;; TODO: invokePrim depending on xf-class
          (format "(%s%s)%s"
            arity-2-ret-type
            arity-2-arg-0-type
            arity-2-arg-1-type))
        ;; Continue chunked for-loop if buf == return from xf
        (.visitJumpInsn Opcodes/IF_ACMPEQ (label 5))        ;; TODO: Can we use IF_ACMPNE for the GOTO and skip label 5?
        ;; Jump to label 1 if buf != return (end for-loop)
        (.visitJumpInsn Opcodes/GOTO (label 1))
        ;; Continuing chunked for-loop
        (.visitLabel (label 5))
        (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
        (.visitIincInsn 4 1)
        (.visitJumpInsn Opcodes/GOTO (label 3))
        ;; Ending the chunked block by assigning c = ch.chunkedMore()
        (.visitLabel (label 4))
        (.visitFrame Opcodes/F_CHOP 1 nil 0 nil)
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitTypeInsn Opcodes/CHECKCAST (Type/getInternalName clojure.lang.IChunkedSeq))
        (invoke-interface clojure.lang.IChunkedSeq "chunkedMore" (format "()%s" iseq-type))
        (.visitVarInsn Opcodes/ASTORE 2)
        ;; Jump to checking buffer + return at label 6
        (.visitJumpInsn Opcodes/GOTO (label 6))

        ;; Handling the non-chunked case
        (.visitLabel (label 2))
        (.visitFrame Opcodes/F_CHOP 1 nil 0 nil)
        ;; Load and call xf
        (.visitVarInsn Opcodes/ALOAD 1)
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitFieldInsn Opcodes/GETFIELD class-name "xf" xf-type)
        (.visitVarInsn Opcodes/ALOAD 1)
        (.visitVarInsn Opcodes/ALOAD 2)
        (invoke-interface clojure.lang.ISeq "first" (format "()%s" obj-type)) ;; TODO: Call firstLong/firstDouble depending on input-type
        (invoke-interface xf-class "invoke"
          (format "(%s%s)%s"
            arity-2-ret-type
            arity-2-arg-0-type
            arity-2-arg-1-type))
        (.visitJumpInsn Opcodes/IF_ACMPEQ (label 7)) ;; TODO: Can we use IF_ACMPNE for the GOTO and skip label 7?
        (.visitJumpInsn Opcodes/GOTO (label 1))
        (.visitLabel (label 7))
        (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
        ;; Load and assign ISeq c = c.more()
        (.visitVarInsn Opcodes/ALOAD 2)
        (invoke-interface clojure.lang.ISeq "more" (format "()%s" iseq-type))
        (.visitVarInsn Opcodes/ASTORE 2)

        ;; Start end of for-loop, checking buffer and returning
        (.visitLabel (label 6))
        (.visitFrame Opcodes/F_SAME 0 nil 0 nil)
        (.visitVarInsn Opcodes/ALOAD 1)
        (invoke-interface buffer-class "isEmpty" "()Z")
        ;; If empty, jump to 8 where the next part of the outer for-loop is setup (c = c.seq)
        (.visitJumpInsn Opcodes/IFNE (label 8))
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitFieldInsn Opcodes/PUTFIELD class-name "s" seq-type)
        ;; Construct the next LazySeq and return
        (.visitVarInsn Opcodes/ALOAD 1)
        (.visitTypeInsn Opcodes/NEW (Type/getInternalName clojure.lang.LazySeq))
        (.visitInsn Opcodes/DUP)
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
        (.visitFieldInsn Opcodes/GETFIELD class-name "xf" xf-type)
        (.visitVarInsn Opcodes/ALOAD 1)
        (invoke-interface xf-class "invoke" (format "(%s)%s" arity-1-ret-type arity-1-arg-0-type))
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
        (.visitMaxs 5 5)
        (.visitEnd))

      (.visitEnd cw)

      (.toByteArray cw))))

(comment

  (require '[clojure.java.io :as io])
  (spit "my-own.class" (generate-xfseq-simple))
  )