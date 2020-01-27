package xfseq;

import clojure.lang.*;

public abstract class AChunkedCons extends ASeq implements IChunkedSeq {

    final IChunk chunk;
    final ISeq more;

    AChunkedCons(IPersistentMap meta, IChunk chunk, ISeq more) {
        super(meta);
        this.chunk = chunk;
        this.more = more;
    }

    abstract AChunkedCons newMeta(IPersistentMap meta);
    abstract AChunkedCons newChunk(IChunk chunk);

    public Obj withMeta(IPersistentMap meta) {
        return meta != this.meta() ? newMeta(meta) : this;
    }

    public Object first() {
        return this.chunk.nth(0);
    }

    public ISeq next() {
        return this.chunk.count() > 1 ? newChunk(this.chunk.dropFirst()) : this.chunkedNext();
    }

    public ISeq more() {
        if (this.chunk.count() > 1) {
            return newChunk(this.chunk.dropFirst());
        } else {
            return this.more == null ? PersistentList.EMPTY : this.more;
        }
    }

    public IChunk chunkedFirst() {
        return this.chunk;
    }

    public ISeq chunkedNext() {
        return this.chunkedMore().seq();
    }

    public ISeq chunkedMore() {
        return this.more == null ? PersistentList.EMPTY : this.more;
    }
}
