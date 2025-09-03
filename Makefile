# ---- Variables ----
CXX      := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -Iinclude
SRC_DIR  := src
OBJ_DIR  := obj
BIN      := ProcessCorrelation

SRCS := $(wildcard $(SRC_DIR)/*.cpp)
OBJS := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRCS))

all: $(BIN)

$(BIN): $(OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) $(BIN)
	rm -f output/*.txt
	rm -f generated_correlations/src/*.cpp
	rm -f generated_correlations/include/*.h
	rm -f generated_correlations/output/*.txt
	
clean-corr:
	rm -f generated_correlations/src/*.cpp
	rm -f generated_correlations/include/*.h
	rm -f generated_correlations/output/*.txt

run: $(BIN)
	./$(BIN)

.PHONY: all clean run

# --- Tests(Catch2) ---

TEST_DIR := tests
TEST_BIN := run_all_tests
TEST_SRCS := $(wildcard $(TEST_DIR)/*.cpp)
TEST_OBJS := $(patsubst $(TEST_DIR)/%.cpp,$(OBJ_DIR)/test_%.o,$(TEST_SRCS))
APP_SRCS_FOR_TESTS := $(filter-out $(SRC_DIR)/main.cpp, $(SRCS))
APP_OBJS_FOR_TESTS := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(APP_SRCS_FOR_TESTS))

$(OBJ_DIR)/test_%.o: $(TEST_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -I$(TEST_DIR) -c $< -o $@

test: $(TEST_BIN)
	./$(TEST_BIN) -s


test-xml: $(TEST_BIN)
	./$(TEST_BIN) --reporter xml --out test_results.xml


test-compact: $(TEST_BIN)
	./$(TEST_BIN) -s -r compact > test_compact.log


$(TEST_BIN): $(APP_OBJS_FOR_TESTS) $(TEST_OBJS)
	$(CXX) $(CXXFLAGS) -I$(TEST_DIR) $^ -o $@

test-clean:
	rm -f $(TEST_BIN)
	rm -f $(OBJ_DIR)/test_*.o
	rm -rf temp_input_dir_for_full_test
	rm -f temp_full_output.txt
	rm -rf output
	rm -f temp_input.tex temp_output.txt
	rm -f test_results.xml test_compact.log

.PHONY: test test-clean test-xml test-compact
