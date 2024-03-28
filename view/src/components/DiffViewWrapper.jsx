// Wrapper of react-diff-view package for showing code of functions
// Author: Lukas Petr

import {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import { PropTypes } from 'prop-types';
import {
  Diff,
  Hunk,
  parseDiff,
  expandFromRawCode,
  Decoration,
  textLinesToHunk,
  tokenize,
  findChangeByOldLineNumber,
  findChangeByNewLineNumber,
  getChangeKey,
  getCorrespondingOldLineNumber,
} from 'react-diff-view';
import refractor from 'refractor';

/** Show whole function if function has max this amount of line. */
const MAX_LINES_THRESHOLD = 25;
/** Amount of lines to show around calling function
 * if function is too long to show whole. */
const LINES_OF_CONTEXT = 10;

function isFunctionLong(startLine, endLine) {
  return endLine - startLine >= MAX_LINES_THRESHOLD;
}

/**
 * Creates a file structure necessary for visualisation of two functions
 * with empty diff. Creates one hunk containing just a single line:
 * the line numbered oldStart in oldCode. newStart is the number of
 * the corresponding line in the new file.
 * @param {String} oldCode - Content of file containing old version of function
 * @returns Returns file structure containing created hunk.
 */
function createFileStructure(oldCode, oldStart, newStart) {
  const linesOfCode = oldCode.split('\n');
  const lineOfCode = linesOfCode[oldStart - 1];
  const hunkFromLine = textLinesToHunk([lineOfCode], oldStart, newStart);
  const file = {
    type: 'modify',
    hunks: [hunkFromLine],
  };
  return file;
}

/**
 * Transform lines of code (to be highlighted) to array which Diff accepts.
 * @param {Number} linesToShow - Lines (old, new) of code which should be highlighted.
 * @returns Returns array of lines to be highlighted acceptable by Diff.
 */
function getLinesToHighlight(linesToShow, hunks) {
  const changesToHighlight = [];
  // Do not highlight when linesToShow[] is set to -1.
  if (!linesToShow) return [];
  if (linesToShow[0] !== -1) {
    const oldChange = findChangeByOldLineNumber(hunks, linesToShow[0]);
    if (oldChange) {
      const oldChangeKey = getChangeKey(oldChange);
      changesToHighlight.push(oldChangeKey);
    }
  }
  if (linesToShow[1] !== -1) {
    const newChange = findChangeByNewLineNumber(hunks, linesToShow[1]);
    if (newChange) {
      const newChangeKey = getChangeKey(newChange);
      changesToHighlight.push(newChangeKey);
    }
  }
  return changesToHighlight;
}

/**
 * Creates a file structure for code visualisation of one version of a function.
 * @param {bool} isOld - True if the version is old version of the function, otherwise false.
 * @param {string} code - Code of a file in which is the function located.
 * @param {Number} start - Line on which the code of the function starts.
 * @param {Number} end - Line on which the code of the function ends.
 * @returns Returns file structure containing code of the function.
 */
function createFileStructForOneSide(isOld, code, start, end) {
  // The structure which react-diff-view uses
  // is described here https://github.com/ecomfe/gitdiff-parser#api
  // (gitdiff-parser is a package which react-diff-view uses).
  let lines = code.split('\n');
  lines = lines.slice(start - 1, end);
  const changes = lines.reduce((result, line, index) => {
    const change = {
      content: line,
      type: isOld ? 'delete' : 'insert',
      lineNumber: start + index,
    };
    if (isOld) {
      change.isDelete = true;
    } else {
      change.isInsert = true;
    }
    result.push(change);
    return result;
  }, []);
  const hunk = {
    oldStart: isOld ? start : 0,
    oldLines: isOld ? end - start + 1 : 0,
    newStart: isOld ? 0 : start,
    newLines: isOld ? 0 : end - start + 1,
    changes,
  };
  hunk.content = `@@ -${hunk.oldStart},${hunk.oldLines} +${hunk.newStart},${hunk.newLines} @@`;
  // Note: Missing fields: oldMode, newMode, similarity, oldRevision, newRevision,
  // Note 2: `type` must be 'modify' so the code is shown on the correct side.
  const file = {
    hunks: [hunk],
    oldEndingNewLine: true,
    newEndingNewLine: true,
    oldPath: '',
    newPath: '',
    type: 'modify',
  };
  return file;
}

function getSyntaxHighlightedTokens(hunks) {
  const options = {
    highlight: true,
    refractor,
    language: 'c',
  };
  return tokenize(hunks, options);
}

/**
 * A component for visualisation of source code of two versions of a function.
 * For the function, we show:
 * - lines containing differences,
 * - the first and the last line of the function.
 *
 * If the function is calling another function (`linesToShow` is set), then
 * - if the function is short (<= MAX_LINES_THRESHOLD), the entire function is shown,
 * - otherwise, only the line with the call surrounded by a certain
 *   amount (LINES_OF_CONTEXT) of lines is shown.
 *
 * For the lines which are not shown, we render buttons that allow user to show these lines.
 * @param {Object} props - Note: The old/new props can be null when showing only one version
 *   of the function.
 * @param {string} props.oldCode - Source code of file containing old version of the function.
 * @param {string} props.newCode - Source code of file containing new version of the function.
 *   If oldCode or newCode is empty string, then we are showing only one version of function.
 * @param {string} props.diff - Output of unified diff of old and new source code.
 * @param {number} props.oldStart - Line where the old version of the function starts.
 * @param {number} props.newStart - Line where the new version of the function starts.
 * @param {number} props.oldEnd - Line where the old version of the function ends.
 * @param {number} props.newEnd - Line where the new version of the function ends.
 * @param {Boolean} [props.showDiff=true] - True to color differences in function.
 * @param {Number} [props.linesToShow] - Lines (old, new) of function which
 *   should be shown and highlighted. Can be undefined/null if no line to show.
 *   Old/new line can be -1 if showing only one version of function.
 */
export default function DiffViewWrapper({
  oldCode,
  newCode,
  diff,
  oldStart,
  newStart,
  oldEnd,
  newEnd,
  showDiff = true,
  linesToShow = null,
}) {
  /* Showing code only for one version of a function? */
  const onlyOneSide = (oldCode === '' || newCode === '');
  /* getting object which Diff component needs */
  const file = useMemo(() => {
    let returnValue;
    if (onlyOneSide) {
      const onlyOld = (newCode === '');
      const code = onlyOld ? oldCode : newCode;
      const start = onlyOld ? oldStart : newStart;
      const end = onlyOld ? oldEnd : newEnd;
      returnValue = createFileStructForOneSide(onlyOld, code, start, end);
    } else if (diff !== '') {
      // passing zip option for better look in split view
      const [fileObject] = parseDiff(diff, { nearbySequences: 'zip' });
      returnValue = fileObject;
    } else {
      returnValue = createFileStructure(oldCode, oldStart, newStart);
    }
    return returnValue;
  }, [diff, oldCode, newCode, oldStart, newStart, oldEnd, newEnd, onlyOneSide]);
    /* hunks of differences/code which will be shown */
  const [hunks, setHunks] = useState(null);
  // Function for adding lines of old code form startLine to endLine
  // which should be shown.
  // TODO - edit to work also if showing only one version of a function.
  // Currently it does not make sense, because only one version is shown
  // in case of macro call, for macro calls we do not know on which line
  // is called next function/macro and we are showing whole body of macro.
  const expandHunks = useCallback(
    (startLine, endLine = startLine) => {
      setHunks((oldHunks) => expandFromRawCode(oldHunks, oldCode, startLine, endLine + 1));
    },
    [oldCode],
  );

  /**
   * Setting hunks (lines) which will be shown.
   * See the component description comment for information on which lines are shown.
   */
  useEffect(() => {
    const addLineWithContext = (line) => {
      const beginning = Math.max(line - LINES_OF_CONTEXT, oldStart);
      const ending = Math.min(line + LINES_OF_CONTEXT, oldEnd);
      expandHunks(beginning, ending);
    };

    const addAllLinesOfFunction = () => {
      expandHunks(oldStart, oldEnd);
    };
    // filters out diff hunks which are not located in selected code
    const getHunksInRange = (oldHunks) => oldHunks.filter(
      (hunk) => hunk.oldStart <= oldEnd
                    && hunk.oldStart + hunk.oldLines >= oldStart,
    );
    const addFirstAndLastLine = () => {
      expandHunks(oldStart);
      expandHunks(oldEnd);
    };

    setHunks(file.hunks);
    // If showing code for onlyOneSide, then right now we are showing the whole function.
    if (onlyOneSide) return;
    addFirstAndLastLine();
    setHunks((oldHunks) => getHunksInRange(oldHunks));
    // Showing function which is called instead of diff of function
    if (linesToShow) {
      if (isFunctionLong(oldStart, oldEnd)) {
        // old calling line
        addLineWithContext(linesToShow[0]);
        // add new calling line in case if it is in different change
        // than old calling line
        const oldLineOfNewCalling = getCorrespondingOldLineNumber(
          file.hunks,
          linesToShow[1],
        );
        if (oldLineOfNewCalling !== linesToShow[0]) {
          addLineWithContext(oldLineOfNewCalling);
        }
      } else {
        addAllLinesOfFunction();
      }
    }
  }, [file, oldCode, oldStart, oldEnd, linesToShow, expandHunks, onlyOneSide]);

  /**
   * Function for rendering/adding expand button on places
   * where are missing lines of code of function.
   */
  const renderHunks = (output, currentHunk, index, allHunks) => {
    const previousHunk = index > 0 ? allHunks[index - 1] : null;

    if (previousHunk) {
      const nextStart = previousHunk.oldStart + previousHunk.oldLines;
      if (nextStart < currentHunk.oldStart) {
        output.push(
          <Decoration key={`expand${currentHunk.content}`}>
            <button
              className="diff-view-expand-button"
              type="button"
              onClick={() => expandHunks(nextStart, currentHunk.oldStart - 1)}
            >
              ↕ Expand lines
            </button>
          </Decoration>,
        );
      }
    }
    output.push(<Hunk hunk={currentHunk} key={currentHunk.content} />);
    return output;
  };

  if (!hunks) {
    return <>Loading...</>;
  }

  return (
    <Diff
      viewType="split"
      hunks={hunks}
      diffType={file.type}
      className={showDiff ? 'show-diff' : 'do-not-show-diff'}
      selectedChanges={getLinesToHighlight(linesToShow, hunks)}
      tokens={getSyntaxHighlightedTokens(hunks)}
    >
      {(arrayOfHunks) => arrayOfHunks.reduce(renderHunks, [])}
    </Diff>
  );
}

DiffViewWrapper.propTypes = {
  oldCode: PropTypes.string,
  newCode: PropTypes.string,
  diff: PropTypes.string.isRequired,
  oldStart: PropTypes.number,
  newStart: PropTypes.number,
  oldEnd: PropTypes.number,
  newEnd: PropTypes.number,
  showDiff: PropTypes.bool,
  linesToShow: PropTypes.arrayOf(PropTypes.number),
};
