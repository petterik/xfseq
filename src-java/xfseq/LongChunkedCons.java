package xfseq;

import clojure.lang.IChunk;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;

public class LongChunkedCons extends AChunkedCons implements ILongSeq {

    private final ILongChunk chunk;

    public LongChunkedCons(IPersistentMap meta, ILongChunk chunk, ISeq more) {
        super(meta, chunk, more);
        this.chunk = chunk;
    }

    public LongChunkedCons(ILongChunk chunk, ISeq more) {
        this(null, chunk, more);
    }


    @Override
    AChunkedCons newMeta(IPersistentMap meta) {
        return new LongChunkedCons(meta, chunk, more);
    }

    @Override
    AChunkedCons newChunk(IChunk chunk) {
        return new LongChunkedCons((ILongChunk)chunk, more);
    }

    @Override
    public long firstLong() {
        return chunk.nthLong(0);
    }
}
