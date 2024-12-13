package chess

import "testing"

func TestPGNError_Error(t *testing.T) {
	err := &PGNError{"test error", 5}
	expected := "test error"
	if err.Error() != expected {
		t.Fatalf("expected %s but got %s", expected, err.Error())
	}
}

func TestPGNError_Is(t *testing.T) {
	err1 := &PGNError{"test error", 5}
	err2 := &PGNError{"test error", 10}
	err3 := &PGNError{"different error", 5}

	if !err1.Is(err2) {
		t.Fatalf("expected errors to be equal")
	}

	if err1.Is(err3) {
		t.Fatalf("expected errors to be different")
	}
}
