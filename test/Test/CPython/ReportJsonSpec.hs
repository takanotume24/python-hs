module Test.CPython.ReportJsonSpec (spec) where

import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (AssertionFailure))
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.ReportJson (reportJson)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "reportJson" $ do
  it "renders passing and failing cases with categories" $ do
    let passingCase =
          CaseReport
            { caseReportId =
                CaseId
                  { caseIdFile = "test_ok.py",
                    caseIdClassName = "TestOk",
                    caseIdMethodName = "test_ok"
                  },
              caseReportFailure = Nothing
            }
        failingCase =
          CaseReport
            { caseReportId =
                CaseId
                  { caseIdFile = "test_fail.py",
                    caseIdClassName = "TestFail",
                    caseIdMethodName = "test_fail"
                  },
              caseReportFailure =
                Just
                  CaseFailure
                    { caseFailureCategory = AssertionFailure,
                      caseFailureMessage = "AssertionError: 1 != 2"
                    }
            }
    reportJson [passingCase, failingCase]
      `shouldBe` unlines
        [ "{",
          "  \"total\": 2,",
          "  \"passed\": 1,",
          "  \"failed\": 1,",
          "  \"cases\": [",
          "    {\"file\":\"test_ok.py\",\"class\":\"TestOk\",\"method\":\"test_ok\",\"status\":\"pass\",\"category\":null,\"message\":null},",
          "    {\"file\":\"test_fail.py\",\"class\":\"TestFail\",\"method\":\"test_fail\",\"status\":\"fail\",\"category\":\"assertion\",\"message\":\"AssertionError: 1 != 2\"}",
          "  ]",
          "}"
        ]
