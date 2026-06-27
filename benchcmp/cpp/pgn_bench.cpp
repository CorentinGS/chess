// Standalone PGN parse benchmark using chess-library (Disservin/chess-library).
//
// Build:
//   g++ -std=c++17 -O3 -DNDEBUG -march=native -flto \
//       -I /tmp/chess-library/include \
//       benchcmp/cpp/pgn_bench.cpp -o benchcmp/cpp/pgn_bench
//
// Run:
//   ./benchcmp/cpp/pgn_bench /path/to/file.pgn [iterations]
//
// Mirrors the Go BenchmarkPGN_Stream_Big{Big,} workload: full SAN parse with
// move validation and a chess::Board maintained across each game.
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <memory>

#include "chess.hpp"

using namespace chess;

class FullVisitor : public pgn::Visitor {
   public:
    void startPgn() override {}
    void header(std::string_view key, std::string_view value) override {
        if (key == "FEN") board.setFen(value);
    }
    void startMoves() override { games_++; }
    void move(std::string_view move, std::string_view) override {
        Move m = uci::parseSan(board, move, moves);
        if (m == Move::NO_MOVE) {
            skipped_++;
            this->skipPgn(true);
            return;
        }
        board.makeMove<true>(m);
        positions_++;
    }
    void endPgn() override { board.setFen(constants::STARTPOS); }

    std::uint64_t games() const { return games_; }
    std::uint64_t positions() const { return positions_; }
    std::uint64_t skipped() const { return skipped_; }

   private:
    Board board;
    Movelist moves;
    std::uint64_t games_ = 0;
    std::uint64_t positions_ = 0;
    std::uint64_t skipped_ = 0;
};

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::fprintf(stderr, "Usage: %s <pgn_file> [iterations]\n", argv[0]);
        return 1;
    }

    const std::filesystem::path file = argv[1];
    const int iterations = argc >= 3 ? std::atoi(argv[2]) : 1;

    std::error_code ec;
    const auto size_bytes = std::filesystem::file_size(file, ec);
    if (ec) {
        std::fprintf(stderr, "stat %s: %s\n", file.c_str(), ec.message().c_str());
        return 1;
    }
    const double size_mb = static_cast<double>(size_bytes) / (1000.0 * 1000.0);

    std::uint64_t total_games = 0;
    std::uint64_t total_pos = 0;
    std::uint64_t total_skipped = 0;
    double best_seconds = 1e9;

    for (int i = 0; i < iterations; i++) {
        FullVisitor vis;
        std::ifstream in(file.string(), std::ios::binary);
        if (!in) {
            std::fprintf(stderr, "open %s failed\n", file.c_str());
            return 1;
        }

        const auto t0 = std::chrono::high_resolution_clock::now();
        pgn::StreamParser parser(in);
        const auto err = parser.readGames(vis);
        const auto t1 = std::chrono::high_resolution_clock::now();

        if (err) {
            std::fprintf(stderr, "parse error: %s\n", err.message().c_str());
            return 1;
        }

        const double seconds =
            std::chrono::duration<double>(t1 - t0).count();
        best_seconds = std::min(best_seconds, seconds);
        total_games = vis.games();
        total_pos = vis.positions();
        total_skipped = vis.skipped();
    }

    const double mbps = size_mb / best_seconds;
    std::printf(
        "file=%s size=%.2fMB games=%llu pos=%llu skipped=%llu time=%.4fs mbps=%.2f iter=%d\n",
        file.filename().string().c_str(), size_mb,
        static_cast<unsigned long long>(total_games),
        static_cast<unsigned long long>(total_pos),
        static_cast<unsigned long long>(total_skipped), best_seconds, mbps,
        iterations);
    return 0;
}