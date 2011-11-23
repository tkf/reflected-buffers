Feature: Reflected Buffers

  Scenario: Texts of original buffer reflects to reflected buffer
    Given I am in clean buffer "*RefBufTest*"
    And I insert "first words"
    And I eval (refbuf/reflect-current-buffer)
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    Then I should see "zeroth words, first words and additional words"
    Given I am in buffer "*RefBufTest*"
    Then I should see "zeroth words, first words and additional words"
    And I press "C-x k"
    Then there is no reflected buffer of "*RefBufTest*"

  Scenario: Different major modes should work with reflected buffer
    Given I am in clean buffer "*RefBufTest*"
    And I enable lisp-mode
    And I insert "first words"
    And I eval (refbuf/with-mode 'text-mode)
    And I insert "zeroth words, "
    And I press "C-e"
    And I insert " and additional words"
    Then I should see "zeroth words, first words and additional words"
    Given I am in buffer "*RefBufTest*"
    Then I should see "zeroth words, first words and additional words"
    And I press "C-x k"
    Then there is no reflected buffer of "*RefBufTest*"
