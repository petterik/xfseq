package xfseq.buffer;

import clojure.lang.ISeq;

public interface IXFSeqBuffer {

    // Note: Changing this from 2, 4 and 8, sees huge difference
    //       in performance (java8).
    //       Setting it to 8 makes it faster on map inc and filter even
    //       compared to vanilla lazy-seqs
    static final int MIN_SIZE = 8;

    boolean isEmpty();

    ISeq toSeq(ISeq more);

    ISeq toTail();

    // Reducing function IFn.

    Object invoke(Object a);

    Object invoke(Object a, Object b);

}
