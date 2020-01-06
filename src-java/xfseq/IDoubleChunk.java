package xfseq;

import clojure.lang.IChunk;

public interface IDoubleChunk extends IChunk {

    double nthDouble(int i);

}
