/-
  Ask.Error - Unified error handling utilities

  Provides consistent error reporting that:
  1. Always logs when logger is available
  2. Always prints with appropriate styling
  3. Distinguishes fatal vs recoverable errors
-/

import Parlance
import Chronicle

namespace Ask.Error

open Parlance

/-- Error severity levels -/
inductive Severity where
  | fatal     -- Causes exit with non-zero code
  | error     -- Logged and printed, but recoverable in REPL
  | warning   -- Less severe, operation continues
  deriving Repr, BEq

/-- Report an error with consistent logging and display.
    This is the core function - always log when possible, always print. -/
def report (logger : Option Chronicle.Logger) (severity : Severity)
    (msg : String) : IO Unit := do
  -- Always log if logger available
  if let some l := logger then
    match severity with
    | .fatal | .error => l.error msg
    | .warning => l.warn msg
  -- Always print with appropriate style
  match severity with
  | .fatal | .error => printError msg
  | .warning => printWarning msg

/-- Report a fatal error and return exit code 1.
    Use in main/CLI context where errors should terminate. -/
def reportFatal (logger : Option Chronicle.Logger) (msg : String) : IO UInt32 := do
  report logger .fatal msg
  pure 1

/-- Report a recoverable error (logs + prints, no exit code).
    Use in REPL context where errors shouldn't terminate the session. -/
def reportError (logger : Option Chronicle.Logger) (msg : String) : IO Unit :=
  report logger .error msg

/-- Report a warning (logs + prints).
    Use for non-critical issues that don't prevent operation. -/
def reportWarning (logger : Option Chronicle.Logger) (msg : String) : IO Unit :=
  report logger .warning msg

end Ask.Error
