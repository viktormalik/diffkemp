// Tests of the Callstack component appearance

import Callstack from '../components/Callstack';
import { MacroFunctionDifference } from './examples';

// Custom query to find a DOM element in the Callstack
// which represents the called function with the given name
Cypress.Commands.addQuery('findCall', (name) => {
  // creating regex to find the DOM element which represents the call

  // space in the name divides name from a kind (macro, type)
  // the space is visualised as `<br />` tag
  // it looks like Cypress interprets the tag as no character
  const expectedContent = name.replace(' ', '');
  // escaping for usage in regex
  const escapedContent = expectedContent.replace(/[()]/g, '\\$&');
  // using regex for finding element which contains an exact match instead of a substring match
  const regex = new RegExp(`^${escapedContent}$`);

  // logic for creating the custom query
  // TODO: check multiple elements with the same content
  //       `contains` returns the first found element
  const getFunction = cy.now('contains', regex);
  return (subject) => getFunction(subject);
});

/** When checking alignment, the coordinates of calls can differ by this amount of pixels. */
const ALIGN_CHECK_DELTA = 0.5;

/**
 * Checks if the call stack is visualised correctly.
 *
 * Meaning of the following terms which are used:
 * - *matching call*: same called function in old and new call stack,
 * - *non-matching calls*: called functions which are not called in both call stacks,
 * - *old call*: called function in old call stack which is not called in new call stack,
 * - *new call*: called function in new call stack which is not called in old call stack.
 *
 * The following checks are done:
 * - names of the called functions should be visualised in **correct order**
 *   (as specified by `expectedVisualisation`),
 * - calls should be **correctly horizontally aligned**:
 *   - all old and matching calls should start at the same place
 *     (have the same left-coordinate as the first old/matching call),
 *   - all new calls should start at the same place
 *     (have the same left-coordinate as the first new call),
 *   - all old calls should end at the same place
 *     (have the right-coordinate equal to new calls left-coordinate),
 *   - all new/matching calls should end at the same place
 *     (have the same right-coordinate as the first new/matching call).
 * - calls should be **correctly vertically aligned**
 *   - next call should start where the previous call ends
 *     (the top-coordinate of the next call should equal to the bottom-coordinate
 *      of the previous call),
 *   - calls before matching call should end at the same position
 *     (have the same bottom-coordinates)
 *
 * For checking alignment, the coordinates of calls can differ
 * by `ALIGN_CHECK_DELTA` amount of pixels.
 *
 * @param {Array} expectedVisualisation  Representation of the expected visualisation.
 * Contains an array of rows that represents calls from the expected call stack.
 * The row is an array which can contain:
 * -  two calls if the call stack is divided because of non-matching calls
 *  `[function name in the old call stack, in the new call stack]`,
 * -  one call if the call stack is not divided (contains matching call)
 *   `[function name]`.
 *
 * In cases when there are more calls in the old/new call stack than in the second one,
 * use an empty string (`''`) to replace the lack of old/new calls.
 */
function checkCallStackAppearance(expectedVisualisation) {
  // the bottom-coordinates of the previous old/new call for checking vertical align.
  // for the 'matching call' are set both coordinates with the same value
  let previousOldBottom;
  let previousNewBottom;
  // the left-coordinates of the first old/new call for checking horizontal align.
  // if the 'matching call' is the first call then `firstOldXStart` is set
  let firstOldLeft;
  let firstNewLeft;
  // the right-coordinate of the first new/matching call to check that
  // all new/matching calls ends at the same place
  let firstNewRight;
  /**
   * Checks if old call is correctly vertically aligned.
   * @param {Object} element DOM element which represents called function.
   * @param {String} name Name of called function.
   */
  const checkOldCallVerticalAlign = (element, name) => {
    if (previousOldBottom !== undefined) {
      expect(element.getBoundingClientRect().top).to.be.closeTo(
        previousOldBottom,
        ALIGN_CHECK_DELTA,
        `the old call '${name}' is correctly vertically aligned (is below the previous call)`,
      );
    }
    previousOldBottom = element.getBoundingClientRect().bottom;
  };
  const checkNewCallVerticalAlign = (element, name) => {
    if (previousNewBottom !== undefined) {
      expect(element.getBoundingClientRect().top).to.be.closeTo(
        previousNewBottom,
        ALIGN_CHECK_DELTA,
        `the new call '${name}' is correctly vertically aligned (is below the previous call)`,
      );
    }
    previousNewBottom = element.getBoundingClientRect().bottom;
  };
  const checkMatchingCallVerticalAlign = (element, name) => {
    if (previousOldBottom !== undefined && previousNewBottom !== undefined) {
      expect(previousOldBottom).to.be.closeTo(
        previousNewBottom,
        ALIGN_CHECK_DELTA,
        `calls before matching call '${name}' are ending (bottom-coordinates) at the same position`,
      );
      expect(element.getBoundingClientRect().top).to.be.closeTo(
        Math.max(previousOldBottom, previousNewBottom),
        ALIGN_CHECK_DELTA,
        `the matching call '${name}' is correctly vertically aligned (is below previous call)`,
      );
    }
    previousOldBottom = element.getBoundingClientRect().bottom;
    previousNewBottom = element.getBoundingClientRect().bottom;
  };
  const checkOldCallHorizontalAlign = (element, name) => {
    if (firstOldLeft === undefined) {
      firstOldLeft = element.getBoundingClientRect().left;
    } else {
      expect(element.getBoundingClientRect().left).to.be.closeTo(
        firstOldLeft,
        ALIGN_CHECK_DELTA,
        `the old call '${name}' is correctly horizontally aligned `
        + '(has the same left-coordinate as the first old/matching call)',
      );
    }
    expect(element.getBoundingClientRect().right).to.be.closeTo(
      firstNewLeft,
      ALIGN_CHECK_DELTA,
      `the old call '${name}' and the new call are adjacent to each other `
      + '(the old call right-coordinate equals to the new call left-coordinate)',
    );
  }; /* checkOldCallHorizontalAlign */
  const checkNewCallHorizontalAlign = (element, name) => {
    if (firstNewLeft === undefined) {
      firstNewLeft = element.getBoundingClientRect().left;
    } else {
      expect(element.getBoundingClientRect().left).to.be.closeTo(
        firstNewLeft,
        ALIGN_CHECK_DELTA,
        `the new call '${name}' is correctly horizontally aligned `
        + '(has the same left-coordinate as the first new call)',
      );
    }
    if (firstNewRight === undefined) {
      firstNewRight = element.getBoundingClientRect().right;
    } else {
      expect(element.getBoundingClientRect().right).to.be.closeTo(
        firstNewRight,
        ALIGN_CHECK_DELTA,
        `the new call '${name}' is correctly horizontally aligned `
        + '(has the same right-coordinate as the first new/matching call)',
      );
    }
  }; /* checkNewCallHorizontalAlign */
  const checkMatchingCallHorizontalAlign = (element, name) => {
    if (firstOldLeft === undefined) {
      firstOldLeft = element.getBoundingClientRect().left;
    } else {
      expect(element.getBoundingClientRect().left).to.be.closeTo(
        firstOldLeft,
        ALIGN_CHECK_DELTA,
        `the matching call '${name}' is correctly horizontally aligned `
        + '(has the same left-coordinate as the first old/matching call)',
      );
    }
    if (firstNewRight === undefined) {
      firstNewRight = element.getBoundingClientRect().right;
    } else {
      expect(element.getBoundingClientRect().right).to.be.closeTo(
        firstNewRight,
        ALIGN_CHECK_DELTA,
        `the matching call '${name}' is correctly horizontally aligned `
        + '(has the same right-coordinate as the first new/matching call)',
      );
    }
  }; /* checkMatchingCallHorizontalAlign */
  const checkMatchingCall = (element, name) => {
    checkMatchingCallVerticalAlign(element, name);
    checkMatchingCallHorizontalAlign(element, name);
  };
  const checkOldCall = (element, name) => {
    checkOldCallVerticalAlign(element, name);
    checkOldCallHorizontalAlign(element, name);
  };
  const checkNewCall = (element, name) => {
    checkNewCallVerticalAlign(element, name);
    checkNewCallHorizontalAlign(element, name);
  };

  expectedVisualisation.forEach((row) => {
    if (row.length === 1) {
      // matching call
      const [callName] = row;
      cy.findCall(callName)
        .then(([element]) => {
          checkMatchingCall(element, callName);
        });
    } else if (row.length === 2) {
      // non-matching calls
      const [oldName, newName] = row;
      // new call has to be checked first because it is necessary to firstly find out
      // what is the new call left-coordinate to later check if
      // it equals to the old call right-coordinate (if they are next to each other)
      if (newName !== '') {
        cy.findCall(newName)
          .then(([element]) => {
            checkNewCall(element, newName);
          });
      }
      if (oldName !== '') {
        cy.findCall(oldName)
          .then(([element]) => {
            checkOldCall(element, oldName);
          });
      }
    } /* else if (row.length === 2) */ else {
      throw new Error(`Test specification error:
      Wrong amount of calls in a 'row' of the \`expectedVisualisation\`: expected 1 or 2 received ${row.length}
      [${row}]`);
    }
  }); /* expectedVisualisation.forEach */
} /* function checkCallStackAppearance */

specify('a simple call stack should be visualised correctly', () => {
  const compFunName = '__put_task_struct';
  const oldCallStack = [{ name: 'free_task' }, { name: 'free_task_struct' }];
  const newCallStack = [{ name: 'free_task' }, { name: 'free_task_struct' }];

  // Expected visualisation:
  // --------------------
  // | _put_task_struct |
  // | free_task        |
  // | free_task_struct |
  // --------------------
  const expectedVisualisation = [
    ['__put_task_struct'],
    ['free_task'],
    ['free_task_struct'],
  ];

  cy.mount(<Callstack
    compFunName={compFunName}
    oldCallStack={oldCallStack}
    newCallStack={newCallStack}
    definitions={{}}
    onSelect={() => {}}
  />);

  checkCallStackAppearance(expectedVisualisation);
});

specify('a complex call stack should be visualised correctly', () => {
  const compFunName = '_cond_resched';
  const oldCallStack = [
    { name: 'preempt_schedule_common' },
    { name: '__schedule' },
    { name: 'smp_processor_id (macro)' },
    { name: 'raw_smp_processor_id (macro)' },
    { name: 'this_cpu_read (macro)' },
    { name: '__pcpu_size_call_return (macro)' },
    { name: 'this_cpu_read_8 (macro)' },
    { name: 'percpu_from_op (macro)' },
  ];
  const newCallStack = [
    { name: 'preempt_schedule_common' },
    { name: '__schedule' },
    { name: 'smp_processor_id (macro)' },
    { name: '__smp_processor_id (macro)' },
    { name: '__this_cpu_read (macro)' },
    { name: 'raw_cpu_read (macro)' },
    { name: '__pcpu_size_call_return (macro)' },
    { name: 'raw_cpu_read_8 (macro)' },
    { name: 'percpu_from_op (macro)' },
  ];

  // Expected visualisation:
  // ----------------------------------------------------------------
  // | _cond_resched                                                |
  // | preempt_schedule_common                                      |
  // | __schedule                                                   |
  // | smp_processor_id (macro)                                     |
  // |--------------------------------------------------------------|
  // | raw_smp_processor_id (macro)    | __smp_processor_id (macro) |
  // | this_cpu_read (macro)           | __this_cpu_read (macro)    |
  // |                                 | raw_cpu_read (macro)       |
  // |--------------------------------------------------------------|
  // | __pcpu_size_call_return (macro)                              |
  // |--------------------------------------------------------------|
  // | this_cpu_read_8 (macro)         | raw_cpu_read_8 (macro)     |
  // |--------------------------------------------------------------|
  // | percpu_from_op (macro)                                       |
  // ----------------------------------------------------------------
  const expectedVisualisation = [
    ['_cond_resched'],
    ['preempt_schedule_common'],
    ['__schedule'],
    ['smp_processor_id (macro)'],
    ['raw_smp_processor_id (macro)', '__smp_processor_id (macro)'],
    ['this_cpu_read (macro)', '__this_cpu_read (macro)'],
    ['', 'raw_cpu_read (macro)'],
    ['__pcpu_size_call_return (macro)'],
    ['this_cpu_read_8 (macro)', 'raw_cpu_read_8 (macro)'],
    ['percpu_from_op (macro)'],
  ];

  cy.mount(<Callstack
    compFunName={compFunName}
    oldCallStack={oldCallStack}
    newCallStack={newCallStack}
    definitions={{}}
    onSelect={() => {}}
  />);
  checkCallStackAppearance(expectedVisualisation);
});

specify('a call stack with macro-function difference should be visualised correctly', () => {
  const expectedVisualisation = [
    ['down_write'],
    ['get_task_struct (macro)', 'get_task_struct'],
  ];

  cy.mount(<Callstack
    compFunName={MacroFunctionDifference.results[0].function}
    oldCallStack={MacroFunctionDifference.results[0].diffs[0]['old-callstack']}
    newCallStack={MacroFunctionDifference.results[0].diffs[0]['new-callstack']}
    definitions={{}}
    onSelect={() => {}}
  />);
  checkCallStackAppearance(expectedVisualisation);
});
