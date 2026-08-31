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
import xfseq.buffer.ObjectBuffer;

import java.util.concurrent.TimeUnit;

/** ObjectBuffer append/flush rows isolated from source and transducer work. */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase2BufferBenchmark {

    @Param({"1", "4", "8", "31", "32", "33", "64", "1000"})
    public int count;

    private Object[] values;
    private ObjectBuffer buffer;

    @Setup
    public void setup() {
        values = Phase2BenchmarkSupport.values(count);
    }

    @Setup(Level.Invocation)
    public void setupInvocation() {
        buffer = new ObjectBuffer();
    }

    @Benchmark
    public void appendAndFlush(Blackhole blackhole) {
        for (Object value : values) {
            buffer.invoke(buffer, value);
        }
        blackhole.consume(Phase2BenchmarkSupport.checksum(buffer.toTail()));
    }
}
