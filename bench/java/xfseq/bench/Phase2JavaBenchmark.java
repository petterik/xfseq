package xfseq.bench;

import clojure.lang.IFn;
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

import java.util.concurrent.TimeUnit;

/** Direct Java-level candidate rows; candidate selection occurs in setup. */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase2JavaBenchmark {

    @Param({
            "java-polymorphic-object-reduced-aware-v2",
            "java-mixed-object-reduced-aware-v2",
            "java-mixed-object-nonreducing-v2",
            "java-dechunked-object-reduced-aware-v2",
            "java-dechunked-object-nonreducing-v2",
            "java-chunked-object-reduced-aware-v2",
            "java-chunked-object-nonreducing-v2"})
    public String candidateId;

    @Param({"list", "vector"})
    public String sourceKind;

    @Param({"0", "1", "4", "8", "31", "32", "33", "64", "1000",
            "10000", "1000000"})
    public int size;

    @Param({"identity", "map", "filter"})
    public String workload;

    private Object source;
    private Object xform;
    private Object nonReducingXform;
    private Phase2BenchmarkSupport.CandidatePlan plan;
    private IFn candidate;

    @Setup
    public void setup() {
        plan = Phase2BenchmarkSupport.candidatePlan(candidateId);
        if (!plan.accepts(sourceKind)) {
            throw new IllegalArgumentException(
                    Phase2BenchmarkSupport.describeCandidateSource(plan, sourceKind));
        }
        source = Phase2BenchmarkSupport.source(sourceKind, size);
        xform = Phase2BenchmarkSupport.xform(workload);
        if (!plan.reducedAware) {
            nonReducingXform = Phase2BenchmarkSupport.nonReducingOperation(workload);
        }
    }

    @Setup(Level.Invocation)
    public void setupInvocation() {
        Object candidateXform = plan.reducedAware ? xform : nonReducingXform;
        // Construction and source-shape validation happen in fixture setup;
        // the timed methods contain only the selected candidate loop and sink.
        candidate = plan.create(candidateXform, source);
    }

    private Object invokeCandidate() {
        return candidate.invoke();
    }

    @Benchmark
    public void loopFirst(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.firstValue(invokeCandidate()));
    }

    @Benchmark
    public void loopPrefix8(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.prefixChecksum(invokeCandidate(), 8));
    }

    @Benchmark
    public void loopTraverse(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.checksum(invokeCandidate()));
    }
}
