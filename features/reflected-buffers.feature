Feature: Reflected Buffers

  Scenario: Texts of original buffer reflects to reflected buffer
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I eval (refbuf/reflect-current-buffer)
    Then buffer "**RefBufTest* (ref)*" exists
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    Then I should see "zeroth words, first words and additional words"
    Given I am in buffer "*RefBufTest*"
    Then I should see "zeroth words, first words and additional words"
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"

  Scenario: Different major modes should work with reflected buffer
    Given I am in clean buffer "*RefBufTest*"
    And I enable lisp-mode
    And I insert "first words"
    And I eval (refbuf/with-mode 'text-mode)
    Then buffer "**RefBufTest* (ref|mode:text-mode)*" exists
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    Then I should see "zeroth words, first words and additional words"
    Given I am in buffer "*RefBufTest*"
    Then I should see "zeroth words, first words and additional words"
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref|mode:text-mode)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"

  Scenario: Killing original kills reflected buffer
    Given I am in clean buffer "*RefBufTest*"
    And I eval (refbuf/reflect-current-buffer)
    Given I am in buffer "*RefBufTest*"
    Then buffer "**RefBufTest* (ref)*" exists
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"

  Scenario: Killing reflected buffer removes buffer local hooks
    Given I am in clean buffer "*RefBufTest*"
    And I eval (refbuf/reflect-current-buffer)
    Then buffer "**RefBufTest* (ref)*" exists
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"
    Then buffer "*RefBufTest*" exists
    Given I am in buffer "*RefBufTest*"
    And I insert "this will not cause error"

  Scenario: Second call just opens the reflected buffers
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I eval (refbuf/reflect-current-buffer)
    Then I should be in buffer "**RefBufTest* (ref)*"
    Given I am in buffer "*RefBufTest*"
    And I eval (refbuf/reflect-current-buffer)
    Then I should be in buffer "**RefBufTest* (ref)*"
    And I should see "first words"

  Scenario: Second call with mode just opens the reflected buffers
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I eval (refbuf/with-mode 'text-mode)
    Then I should be in buffer "**RefBufTest* (ref|mode:text-mode)*"
    Given I am in buffer "*RefBufTest*"
    And I eval (refbuf/with-mode 'text-mode)
    Then I should be in buffer "**RefBufTest* (ref|mode:text-mode)*"
    And I should see "first words"

  Scenario: Three reflected buffers
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I am in buffer "*RefBufTest*"
    And I eval (refbuf/with-mode 'text-mode)
    And I am in buffer "*RefBufTest*"
    And I eval (refbuf/with-mode 'lisp-mode)
    And I am in buffer "*RefBufTest*"
    Then buffer "**RefBufTest* (ref|mode:text-mode)*" exists
    Then buffer "**RefBufTest* (ref|mode:lisp-mode)*" exists
    And I press "C-a"
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    And I am in buffer "**RefBufTest* (ref|mode:text-mode)*"
    Then I should see "zeroth words, first words and additional words"
    And I am in buffer "**RefBufTest* (ref|mode:lisp-mode)*"
    Then I should see "zeroth words, first words and additional words"
    And I am in buffer "*RefBufTest*"
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref|mode:text-mode)*" does not exist
    Then buffer "**RefBufTest* (ref|mode:lisp-mode)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"

  Scenario: kill-all-local-variables cant kill me
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I eval (refbuf/reflect-current-buffer)
    Then buffer "**RefBufTest* (ref)*" exists
    Given I am in buffer "**RefBufTest* (ref)*"
    And I eval (kill-all-local-variables)
    Given I am in buffer "*RefBufTest*"
    And I eval (kill-all-local-variables)
    Given I am in buffer "**RefBufTest* (ref)*"
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    Then I should see "zeroth words, first words and additional words"
    Given I am in buffer "*RefBufTest*"
    Then I should see "zeroth words, first words and additional words"
    And I press "C-x k"
    Then buffer "**RefBufTest* (ref)*" does not exist
    Then there is no reflected buffer of "*RefBufTest*"
    And I insert "this will not cause error"

  Scenario: Saving reflected buffer saves the original
    Given I open a temp file in RefBufTestDir
    And I insert "first words"
    And I eval (refbuf/reflect-current-buffer)
    And I press "C-x C-s"
    Then I should see message "refbuf: Saved original buffer 'RefBufTestFile"
    And I press "C-x b"
    Then this buffer is not modified
