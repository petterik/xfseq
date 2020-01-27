package xfseq;

import clojure.lang.*;

public class DoubleChunkedCons extends AChunkedCons implements IDoubleSeq {

    private final IDoubleChunk chunk;

    public DoubleChunkedCons(IPersistentMap meta, IDoubleChunk chunk, ISeq more) {
        super(meta, chunk, more);
        this.chunk = chunk;
    }

    public DoubleChunkedCons(IDoubleChunk chunk, ISeq more) {
        this(null, chunk, more);
    }

    @Override
    public double firstDouble() {
        return chunk.nthDouble(0);
    }

    @Override
    AChunkedCons newMeta(IPersistentMap meta) {
        return new DoubleChunkedCons(meta, chunk, more);
    }

    @Override
    AChunkedCons newChunk(IChunk chunk) {
        return new DoubleChunkedCons((IDoubleChunk)chunk, more);
    }
}
