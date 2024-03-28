// Unit tests of DiffViewWrapper component
// Author: Lukas Petr

import {
  cleanup, render, screen, within,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import userEvent from '@testing-library/user-event';

import DiffViewWrapper from '../components/DiffViewWrapper';
import Difference from '../components/Difference';

/**
 * Helper function to test if shown line number and line of code match
 * to original line numbering.
 * @param {string} oldCode Old source of code.
 * @param {string} newCode New source of code.
 * @param {Function} oldLineCallback Function which is called for each line of
 * old code, function receives shown line number and shown line of code.
 * @param {Function} newLineCallback Same as oldLineCallback but for new code.
 */
function testCodeNumberMatch(
  oldCode,
  newCode,
  oldLineCallback,
  newLineCallback,
) {
  const oldCodeLines = oldCode.split('\n');
  const newCodeLines = newCode.split('\n');
  const rows = screen.getAllByRole('row');
  rows.forEach((row) => {
    const cells = within(row).getAllByRole('cell');
    // is row with code
    if (cells.length !== 4) {
      return;
    }
    // old lines check
    if (cells[0].textContent !== '') {
      const lineNumber = Number(cells[0].textContent);
      const lineCode = cells[1].textContent;
      try {
        expect(lineCode).toBe(oldCodeLines[lineNumber - 1]);
      } catch (e) {
        e.message += `\nReceived line number in old file: ${lineNumber}`;
        throw e;
      }
      oldLineCallback(lineNumber, lineCode);
    }
    // new lines check
    if (cells[2].textContent !== '') {
      const lineNumber = Number(cells[2].textContent);
      const lineCode = cells[3].textContent;
      try {
        expect(lineCode).toBe(newCodeLines[lineNumber - 1]);
      } catch (e) {
        e.message += `\nReceived line number in new file: ${lineNumber}`;
        throw e;
      }
      newLineCallback(lineNumber, lineCode);
    }
  });
}

describe('testing a differing function visualisation', () => {
  const oldStart = 766;
  const newStart = 746;
  const oldEnd = 775;
  const newEnd = 757;
  /* eslint-disable no-tabs */
  const diff = `--- /tmp/tmp47pt0hsi/1	2023-03-16 17:36:00.611068650 +0100
+++ /tmp/tmp47pt0hsi/2	2023-03-16 17:36:00.611068650 +0100
@@ -770,4 +750,6 @@
 
-	if (!(flags & DEQUEUE_SAVE))
+	if (!(flags & DEQUEUE_SAVE)) {
 		sched_info_dequeued(rq, p);
+		psi_dequeue(p, flags & DEQUEUE_SLEEP);
+	}
`;
  /* eslint-enable no-tabs */

  /* eslint-disable no-tabs */
  const oldCode = `${'\n'.repeat(oldStart - 1)
  }static inline void dequeue_task(struct rq *rq, struct task_struct *p, int flags)
{
	if (!(flags & DEQUEUE_NOCLOCK))
		update_rq_clock(rq);

	if (!(flags & DEQUEUE_SAVE))
		sched_info_dequeued(rq, p);

	p->sched_class->dequeue_task(rq, p, flags);
}`;
  /* eslint-enable no-tabs */

  /* eslint-disable no-tabs */
  const newCode = `${'\n'.repeat(newStart - 1)
  }static inline void dequeue_task(struct rq *rq, struct task_struct *p, int flags)
{
	if (!(flags & DEQUEUE_NOCLOCK))
		update_rq_clock(rq);

	if (!(flags & DEQUEUE_SAVE)) {
		sched_info_dequeued(rq, p);
		psi_dequeue(p, flags & DEQUEUE_SLEEP);
	}

	p->sched_class->dequeue_task(rq, p, flags);
}`;
  /* eslint-enable no-tabs */

  const setup = () => {
    render(
      <DiffViewWrapper
        oldCode={oldCode}
        diff={diff}
        oldStart={oldStart}
        newStart={newStart}
        oldEnd={oldEnd}
        showDiff
      />,
    );
  };

  afterEach(() => {
    cleanup();
  });

  test('first shown line should be first line of function', () => {
    setup();
    const rows = screen.getAllByRole('row');
    const cells = within(rows[0]).getAllByRole('cell');
    // in old part
    expect(cells[1]).toHaveTextContent(
      /^static inline void dequeue_task\(struct rq \*rq, struct task_struct \*p, int flags\)$/,
    );
    // in new part
    expect(cells[3]).toHaveTextContent(
      /^static inline void dequeue_task\(struct rq \*rq, struct task_struct \*p, int flags\)$/,
    );
  });

  test('last shown line should be last line of function', () => {
    setup();
    const rows = screen.getAllByRole('row');
    const cells = within(rows[rows.length - 1]).getAllByRole('cell');
    // in old part
    expect(cells[1]).toHaveTextContent(/^}$/);
    // in new part
    expect(cells[3]).toHaveTextContent(/^}$/);
  });

  test('all differing lines should be shown', () => {
    setup();
    expect(
      screen.getByText((content, element) => (
        element.tagName === 'TD'
          /* eslint-disable-next-line no-tabs */
          && element.textContent === '	if (!(flags & DEQUEUE_SAVE))'
      )),
    ).toHaveClass('diff-code-delete');
    expect(
      screen.getByText((content, element) => (
        element.tagName === 'TD'
          /* eslint-disable-next-line no-tabs */
          && element.textContent === '	if (!(flags & DEQUEUE_SAVE)) {'
      )),
    ).toHaveClass('diff-code-insert');
    expect(
      screen.getByText((content, element) => (
        element.tagName === 'TD'
          /* eslint-disable-next-line no-tabs */
          && element.textContent === '		psi_dequeue(p, flags & DEQUEUE_SLEEP);'
      )),
    ).toHaveClass('diff-code-insert');
    expect(
      screen.getByText((content, element) => element.tagName === 'TD'
        /* eslint-disable-next-line no-tabs */
        && element.textContent === '	}'),
    ).toHaveClass('diff-code-insert');
  });

  test('shown line numbers should match to shown code', () => {
    setup();
    testCodeNumberMatch(
      oldCode,
      newCode,
      () => {},
      () => {},
    );
  });

  test('after all code expansion whole functions should be visible and line numbers should match code', async () => {
    setup();
    const LOOP_LIMIT = 5;
    let loop = 0;
    let btns = screen.queryAllByText(/expand/i);
    // clicking on expand buttons one by one
    while (btns.length > 0) {
      loop += 1;
      // eslint-disable-next-line no-await-in-loop
      await userEvent.click(btns[0]);
      // endless loop/buttons check
      if (loop > LOOP_LIMIT) {
        throw new Error('looks like endless loop');
      }
      btns = screen.queryAllByText(/expand/i);
    }

    let expectedOldLineNum = oldStart;
    let expectedNewLineNum = newStart;
    testCodeNumberMatch(
      oldCode,
      newCode,
      // testing order of lines and checking whether any are missing
      (lineNumber) => {
        expect(lineNumber).toBe(expectedOldLineNum);
        expectedOldLineNum += 1;
      },
      (lineNumber) => {
        expect(lineNumber).toBe(expectedNewLineNum);
        expectedNewLineNum += 1;
      },
    );
    // testing if shown whole function
    expect(expectedOldLineNum).toBe(oldEnd + 1);
    expect(expectedNewLineNum).toBe(newEnd + 1);
  });
}); /* describe - diff visualisation */

describe('testing a caller function visualisation', () => {
  const oldStart = 176;
  const newStart = 180;
  const oldEnd = 197;
  const newEnd = 201;
  const calling = [190, 194];

  const diff = '';

  /* eslint-disable no-tabs */
  const oldCode = `${'\n'.repeat(oldStart - 1)
  }static void call_usermodehelper_exec_work(struct work_struct *work)
{
	struct subprocess_info *sub_info =
		container_of(work, struct subprocess_info, work);

	if (sub_info->wait & UMH_WAIT_PROC) {
		call_usermodehelper_exec_sync(sub_info);
	} else {
		pid_t pid;
		/*
		 * Use CLONE_PARENT to reparent it to kthreadd; we do not
		 * want to pollute current->children, and we need a parent
		 * that always ignores SIGCHLD to ensure auto-reaping.
		 */
		pid = kernel_thread(call_usermodehelper_exec_async, sub_info,
				    CLONE_PARENT | SIGCHLD);
		if (pid < 0) {
			sub_info->retval = pid;
			umh_complete(sub_info);
		}
	}
}`;
  /* eslint-enable no-tabs */

  /* eslint-disable no-tabs */
  const newCode = `${'\n'.repeat(newStart - 1)
  }static void call_usermodehelper_exec_work(struct work_struct *work)
{
	struct subprocess_info *sub_info =
		container_of(work, struct subprocess_info, work);

	if (sub_info->wait & UMH_WAIT_PROC) {
		call_usermodehelper_exec_sync(sub_info);
	} else {
		pid_t pid;
		/*
		 * Use CLONE_PARENT to reparent it to kthreadd; we do not
		 * want to pollute current->children, and we need a parent
		 * that always ignores SIGCHLD to ensure auto-reaping.
		 */
		pid = kernel_thread(call_usermodehelper_exec_async, sub_info,
				    CLONE_PARENT | SIGCHLD);
		if (pid < 0) {
			sub_info->retval = pid;
			umh_complete(sub_info);
		}
	}
}`;
  /* eslint-enable no-tabs */

  const setup = () => {
    render(
      <DiffViewWrapper
        oldCode={oldCode}
        diff={diff}
        oldStart={oldStart}
        newStart={newStart}
        oldEnd={oldEnd}
        showDiff={false}
        linesToShow={calling}
      />,
    );
  };
  test('line where function is called should be visible', () => {
    setup();
    const callingLines = screen.getAllByText((content, element) => (
      element.tagName === 'TD'
        /* eslint-disable-next-line no-tabs */
        && element.textContent === '		pid = kernel_thread(call_usermodehelper_exec_async, sub_info,'
    ));

    expect(callingLines).toHaveLength(2);
    expect(callingLines[0]).toHaveClass('diff-code-selected');
    expect(callingLines[1]).toHaveClass('diff-code-selected');
  });
  test('for visible lines should match line numbers with code', () => {
    setup();
    testCodeNumberMatch(
      oldCode,
      newCode,
      () => {},
      () => {},
    );
  });

  // Integration testing from Difference componenent.
  test('code of calls which are only on one side of call stack should be visualised correctly', async () => {
    // Note: For these calls should be shown code of the function/macro and it should
    // be shown on correct side (left/right), the other side should be left emppty.

    // Note: Reusing parts from previous test case.
    const example = {
      results: [{
        // compared function
        function: 'cmp_fun',
        diffs: [{
          // differing function
          function: 'diff_fun',
          'old-callstack': [
            { name: 'left (macro)', line: 5, file: 'file1.c' },
            { name: 'diff_fun', line: calling[0], file: 'left_file.c' },
          ],
          'new-callstack': [
            { name: 'right (macro)', line: 5, file: 'file1.c' },
            { name: 'diff_fun', line: calling[1], file: 'right_file.c' },
          ],
        }],
      }],
      definitions: {
        left: {
          kind: 'macro',
          old: { line: oldStart, file: 'left_file.c', 'end-line': oldEnd },
          diff: false,
        },
        right: {
          kind: 'macro',
          new: { line: newStart, file: 'right_file.c', 'end-line': newEnd },
          diff: false,
        },
      },
    };

    // Mocking getFile to return content of file.
    const getFile = jest.fn(async (filePath) => {
      let content = filePath;
      if (filePath === 'old-src/left_file.c') {
        content = oldCode;
      } else if (filePath === 'new-src/right_file.c') {
        content = newCode;
      }
      return content;
    });

    render(
      <Difference
        compare={example.results[0].function}
        diff={example.results[0].diffs[0]}
        definitions={example.definitions}
        getFile={getFile}
        oldFolder=""
        newFolder=""
      />,
    );

    const callstack = within(screen.getByTestId('callstack'));

    // Selecting function which is only in old call stack.
    userEvent.click(callstack.getByTitle('left (macro)'));
    // Waiting for update.
    await screen.findByText(/left_file.c/);
    // Tesing that the whole code of function is shown
    // and it is shown on left side and the right side is empty.
    let expectedOldLineNum = oldStart;
    testCodeNumberMatch(
      oldCode,
      '',
      // Testing order of lines and checking whether any are missing.
      (lineNumber) => {
        expect(lineNumber).toBe(expectedOldLineNum);
        expectedOldLineNum += 1;
      },
      () => {},
    );
    // Testing if shown whole function.
    expect(expectedOldLineNum).toBe(oldEnd + 1);

    // Selecting function which is only in new call stack.
    userEvent.click(callstack.getByTitle('right (macro)'));
    // Waiting for update.
    await screen.findByText(/right_file.c/);
    // Tesing that the whole code of function is shown
    // and it is shown on right side and the left side is empty.
    let expectedNewLineNum = newStart;
    testCodeNumberMatch(
      '',
      newCode,
      () => {},
      // Testing order of lines and checking whether any are missing.
      (lineNumber) => {
        expect(lineNumber).toBe(expectedNewLineNum);
        expectedNewLineNum += 1;
      },
    );
    // Testing if shown whole function.
    expect(expectedNewLineNum).toBe(newEnd + 1);
  });
});
