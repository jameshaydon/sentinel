-- | Askable predicate declarations for the Air Canada example.
--
-- Askable predicates represent facts that can only be established by asking the
-- user for confirmation. The solver will block on these when they are needed
-- but not yet established, allowing the LLM agent to ask the user and record
-- their response.
module Examples.AirCanada.Askables
  ( airCanadaAskables,
  )
where

import Pre
import Sentinel.Solver.Askable
  ( AskableDecl (..),
    AskableRegistry,
    EvidenceType (..),
    declareAskable,
    emptyAskableRegistry,
  )

-- | Askable predicates for Air Canada refund policy.
airCanadaAskables :: AskableRegistry
airCanadaAskables =
  emptyAskableRegistry
    & declareAskable confirmsCancellationDecl
    & declareAskable claimsMedicalEmergencyDecl
    & declareAskable claimsBereavementDecl
    & declareAskable acceptsVoucherTermsDecl
    & declareAskable confirmsUploadDocumentationDecl

--------------------------------------------------------------------------------
-- Askable Declarations
--------------------------------------------------------------------------------

-- | User confirms they understand their booking will be cancelled.
--
-- This is required before any refund can be processed. The user must
-- acknowledge that their booking will become invalid.
--
-- Predicate: user_confirms_cancellation(U)
confirmsCancellationDecl :: AskableDecl
confirmsCancellationDecl =
  AskableDecl
    { predicate = "user_confirms_cancellation",
      arity = 1,
      questionTemplate =
        "Do you understand that processing this refund will cancel your booking "
          <> "and the ticket will no longer be valid for travel?",
      evidenceType = UserStatement,
      description = "User confirms understanding that refund cancels booking"
    }

-- | User claims a medical emergency prevents them from traveling.
--
-- For Basic Economy tickets, a medical emergency (with documentation) may
-- qualify for a travel voucher instead of no refund.
--
-- Predicate: user_claims_medical_emergency(U)
claimsMedicalEmergencyDecl :: AskableDecl
claimsMedicalEmergencyDecl =
  AskableDecl
    { predicate = "user_claims_medical_emergency",
      arity = 1,
      questionTemplate =
        "Are you requesting this refund due to a medical emergency "
          <> "that prevents you from traveling?",
      evidenceType = UserStatement,
      description = "User claims medical emergency"
    }

-- | User claims a bereavement situation.
--
-- Bereavement applies to death of passenger, immediate family member,
-- or travel companion. Requires death certificate documentation.
--
-- Predicate: user_claims_bereavement(U)
claimsBereavementDecl :: AskableDecl
claimsBereavementDecl =
  AskableDecl
    { predicate = "user_claims_bereavement",
      arity = 1,
      questionTemplate =
        "Are you requesting this refund due to bereavement "
          <> "(death of passenger, immediate family member, or travel companion)?",
      evidenceType = UserStatement,
      description = "User claims bereavement circumstances"
    }

-- | User accepts voucher program terms and conditions.
--
-- When receiving a travel voucher instead of a cash refund, the user
-- must accept the voucher program terms (e.g., 1-year validity).
--
-- Predicate: user_accepts_voucher_terms(U)
acceptsVoucherTermsDecl :: AskableDecl
acceptsVoucherTermsDecl =
  AskableDecl
    { predicate = "user_accepts_voucher_terms",
      arity = 1,
      questionTemplate =
        "Do you accept the travel voucher terms? The voucher will be valid "
          <> "for 1 year from date of issue and can be used for any Air Canada flight.",
      evidenceType = ExplicitConfirmation,
      description = "User accepts voucher program T&C"
    }

-- | User confirms they will upload supporting documentation.
--
-- Required for special circumstances (medical, bereavement, jury duty, etc.)
-- where documentation is needed to process the refund.
--
-- Predicate: user_confirms_documentation(U, DocType)
-- Arguments:
--   {0} = User
--   {1} = Document type (e.g., "medical_certificate", "death_certificate")
confirmsUploadDocumentationDecl :: AskableDecl
confirmsUploadDocumentationDecl =
  AskableDecl
    { predicate = "user_confirms_documentation",
      arity = 2,
      questionTemplate =
        "Can you provide a {1} to support your request? "
          <> "This documentation is required to process your refund.",
      evidenceType = DocumentUpload "{1}",
      description = "User confirms they will upload required documentation"
    }
