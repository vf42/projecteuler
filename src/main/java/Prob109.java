import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * https://projecteuler.net/problem=109
 */

public class Prob109 {

    private static enum ScoreType {
        S(1), D(2), T(3);
        private int multiplier;
        ScoreType(int multiplier) {
            this.multiplier = multiplier;
        }
        public int multiplier() {
            return multiplier;
        }
    }

    private static class Score {
        ScoreType type;
        int score;
        Score(ScoreType type, int score) {
            this.type = type;
            this.score = score;
        }
        static Score S(int score) {
            return new Score(ScoreType.S, score);
        }
        static Score D(int score) {
            return new Score(ScoreType.D, score);
        }
        static Score T(int score) {
            return new Score(ScoreType.T, score);
        }
        int value() {
            return score * type.multiplier();
        }
        public String toString() {
            return type.toString() + score;
        }

        @Override
        public int hashCode() {
            return 100 * type.multiplier() + score;
        }
    }

    private static class Checkout {
        Score finisher;
        List<Score> hits;
        Checkout(Score finisher, List<Score> hits) {
            this.finisher = finisher;
            this.hits = hits;
            this.hits.sort(Comparator.comparing(Score::hashCode));
        }

        @Override
        public String toString() {
            var sb = new StringBuilder();
            for (Score h : hits) {
                sb.append(h.toString()).append(' ');
            }
            return sb.append(finisher.toString()).append(' ').append(this.hashCode()).toString();
        }

        @Override
        public int hashCode() {
            int hc = 1000000 * finisher.value();
            if (hits.size() == 2) {
                hc += hits.get(1).hashCode() * 1000;
            }
            if (hits.size() >= 1) {
                hc += hits.get(0).hashCode();
            }
            return hc;
        }

        @Override
        public boolean equals(Object obj) {
            return this.hashCode() == obj.hashCode();
        }
    }

    private static IntStream allValues() {
        return IntStream.concat(IntStream.range(1, 21), IntStream.of(25));
    }

    private static Stream<Score> allScores() {
        var allSingles = allValues().mapToObj(x -> Score.S(x));
        var allDoubles = allValues().mapToObj(x -> Score.D(x));
        var allTriples = IntStream.range(1, 21).mapToObj(x -> Score.T(x));
        return Stream.concat(Stream.concat(allSingles, allDoubles), allTriples).sorted(Comparator.comparing(Score::value));
    }

    private static void traverse(int remainder, Score finisher, LinkedList<Score> hits, Set<Checkout> results) {
        // Exit conditions.
        if (remainder == 0) {
            results.add(new Checkout(finisher, (List<Score>) hits.clone()));
            return;
        } else if (remainder > 0 && hits.size() < 2) {
            // Possible next hits.
            var nextHits = allScores().filter(s -> s.value() <= remainder).toArray(Score[]::new);
            for (Score nh : nextHits) {
                hits.push(nh);
                traverse(remainder - nh.value(), finisher, hits, results);
                hits.pop();
            }
        }
    }

    private static int checkoutOptions(int score) {
        // Get list of all possible finishing doubles.
        var finishingDoubles = allValues()
                .filter(x -> 2 * x <= score)
                .mapToObj(x -> Score.D(x)).toArray(Score[]::new);
        // Traverse through all possible scores.
        var results = new HashSet<Checkout>();
        for (Score fd : finishingDoubles) {
            traverse(score - fd.value(), fd, new LinkedList<>(), results);
        }
        if (System.getenv().containsKey("DEBUG")) {
            for (Checkout r : results) {
                System.out.println(r.toString());
            }
        }
        return results.size();
    }

    public static void main(String[] args) {
        var startT = System.currentTimeMillis();

        var result = 0;
        for (var score = 2; score < Integer.parseInt(args[0]); score++) {
            result += checkoutOptions(score);
        }
        System.out.println("Result: " + result);

        var execT = System.currentTimeMillis() - startT;
        System.out.println("Time: "+ execT / 1000.0);
    }
}
