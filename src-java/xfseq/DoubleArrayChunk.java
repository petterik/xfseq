package xfseq;

import clojure.lang.*;

import java.io.Serializable;

public class DoubleArrayChunk implements IDoubleChunk, Serializable {

    private final double[] array;
    private final int offset;
    private final int end;

    public DoubleArrayChunk(double[] array, int offset, int end) {
        this.array = array;
        this.offset = offset;
        this.end = end;
    }

    @Override
    public IChunk dropFirst() {
        if(offset == end)
            throw new IllegalStateException("dropFirst of empty chunk");
        return new DoubleArrayChunk(array, offset + 1, end);
    }

    @Override
    public Object reduce(IFn f, Object start) {
        Object ret = f.invoke(start, array[offset]);
        if (RT.isReduced(ret))
            return ret;
        for (int i = offset + 1; i < end; i++) {
            ret = f.invoke(ret, array[i]);
            if (RT.isReduced(ret)) {
                return ret;
            }
        }
        return ret;
    }


    @Override
    public double nthDouble(int i) {
        return array[offset + i];
    }

    @Override
    public Object nth(int i) {
        return nthDouble(i);
    }

    @Override
    public Object nth(int i, Object notFound) {
        if (i >= 0 && i < count()) {
            return array[offset + i];
        } else {
            return notFound;
        }
    }

    @Override
    public int count() {
        return end - offset;
    }
}
