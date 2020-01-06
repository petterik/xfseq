package xfseq;

import clojure.lang.IChunk;

public interface ILongChunk extends IChunk {

    long nthLong(int i);

}
