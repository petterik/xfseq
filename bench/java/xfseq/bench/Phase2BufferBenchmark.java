package xfseq.bench;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;
import clojure.lang.AFn;
import clojure.lang.ArrayChunk;
import clojure.lang.ChunkedCons;
import clojure.lang.ISeq;
import xfseq.buffer.IXFSeqBuffer;
import xfseq.buffer.ObjectBuffer;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

/** ObjectBuffer append/flush rows isolated from source and transducer work. */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase2BufferBenchmark {

    @Param({"1", "4", "8", "31", "32", "33", "64", "1000"})
    public int count;

    /**
     * The current policy retains the production ObjectBuffer.  all-chunk is a
     * benchmark-only alternative that emits ArrayChunk for every non-empty
     * batch, including one-to-four values; it never changes production code.
     */
    @Param({"current", "all-chunk"})
    public String policy;

    private Object[] values;
    private IXFSeqBuffer buffer;

    @Setup
    public void setup() {
        values = Phase2BenchmarkSupport.values(count);
    }

    @Setup(Level.Invocation)
    public void setupInvocation() {
        buffer = "current".equals(policy)
                ? new ObjectBuffer()
                : new AllChunkBuffer();
    }

    @Benchmark
    public void appendAndFlush(Blackhole blackhole) {
        for (Object value : values) {
            buffer.invoke(buffer, value);
        }
        blackhole.consume(Phase2BenchmarkSupport.checksum(buffer.toTail()));
    }

    /** Minimal benchmark-only all-chunk policy with the same buffer contract. */
    private static final class AllChunkBuffer extends AFn
            implements IXFSeqBuffer {
        private Object[] values = new Object[32];
        private int count;

        @Override
        public Object invoke(Object accumulator) {
            return accumulator;
        }

        @Override
        public Object invoke(Object accumulator, Object value) {
            if (count == values.length) {
                Object[] larger = new Object[values.length * 2];
                System.arraycopy(values, 0, larger, 0, count);
                values = larger;
            }
            values[count++] = value;
            return accumulator;
        }

        @Override
        public boolean isEmpty() {
            return count == 0;
        }

        @Override
        public ISeq toSeq(ISeq more) {
            ISeq result = more;
            int offset = count;
            while (offset > 0) {
                int start = Math.max(0, offset - 32);
                Object[] chunkValues = Arrays.copyOfRange(values, start, offset);
                result = new ChunkedCons(new ArrayChunk(chunkValues, 0,
                        offset - start), result);
                offset = start;
            }
            Arrays.fill(values, 0, count, null);
            count = 0;
            if (values.length > 32) {
                values = new Object[32];
            }
            return result;
        }

        @Override
        public ISeq toTail() {
            return toSeq(null);
        }
    }
}
