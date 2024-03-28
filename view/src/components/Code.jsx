// Visualisation of code of function.
// Author: Lukas Petr

import { useState, useEffect } from 'react';
import { ErrorBoundary } from 'react-error-boundary';
import path from 'path-browserify';
import { PropTypes } from 'prop-types';

import Alert from 'react-bootstrap/Alert';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';

import DiffViewWrapper from './DiffViewWrapper';

const OLD_SRC_DIR = 'old-src';
const NEW_SRC_DIR = 'new-src';
const DIFF_DIRECTORY = 'diffs';

/**
 * Code/function preparation and visualisation.
 * @param {Object} props
 * @param {Object} props.specification - Specification of code to be shown.
 *   If only one version (old/new) of function is supposed to be shown then the fields
 *   for the other version should be null.
 * @param {string} props.specification.oldSrc - Path to the source file in which
 *                                              is located old version of function.
 * @param {string} props.specification.newSrc - Path to source file of the
 *                                              new version of function.
 * @param {string} props.specification.diff - Path to file with diff of function.
 * @param {Number} props.specification.oldStart - The file line number where the old version
 *                                                of the function starts.
 * @param {Number} props.specification.newStart - The file line number for the new version.
 * @param {Number} props.specification.oldEnd - The file line number where the old version
 *                                              of the function ends.
 * @param {Number} [props.specification.calling] - Tuple (old, new) of line numbers where are called
 *                                                 next functions which should be highlighted
 *                                                 or undefined if it is differing function.
 * @param {string} props.oldFolder - Name of snapshot folder with old version of project.
 * @param {string} props.newFolder - Name of snapshot folder with new version of project.
 * @param {Function} props.getFile - Function for getting file.
 * @returns Return code visualisation component.
 */
export default function Code({
  specification, oldFolder, newFolder, getFile,
}) {
  const [oldCode, setOldCode] = useState(null);
  const [newCode, setNewCode] = useState(null);
  const [diff, setDiff] = useState(null);
  // Getting content of source and diff file.
  useEffect(() => {
    // variable for handling race conditions
    let ignoreFetchedFiles = false;

    const getOldCode = async () => {
      const oldCodeFile = await getFile(
        path.join(OLD_SRC_DIR, specification.oldSrc),
      );
      if (ignoreFetchedFiles) return;
      setOldCode(oldCodeFile);
    };
    const getNewCode = async () => {
      const newCodeFile = await getFile(
        path.join(NEW_SRC_DIR, specification.newSrc),
      );
      if (ignoreFetchedFiles) return;
      setNewCode(newCodeFile);
    };
    const getDiff = async () => {
      const diffFile = await getFile(
        path.join(DIFF_DIRECTORY, specification.diff),
      );
      if (ignoreFetchedFiles) return;
      setDiff(diffFile);
    };

    if (specification) {
      if (specification.oldSrc) getOldCode();
      else setOldCode('');
      if (specification.newSrc) getNewCode();
      else setNewCode('');

      if (specification.diff) {
        getDiff();
      } else {
        setDiff('');
      }
    }
    return () => {
      ignoreFetchedFiles = true;
    };
  }, [specification, getFile]);
  if (oldCode === null || newCode === null || diff == null) {
    return null;
  }
  return (
    <div>
      {/* showing info about location of file */}
      <Row className="py-2 border border-primary rounded-top text-bg-primary">
        <Col>{specification.oldSrc && path.join(oldFolder, specification.oldSrc)}</Col>
        <Col>{specification.newSrc && path.join(newFolder, specification.newSrc)}</Col>
      </Row>
      {/* showing code of function */}
      <Row className="border border-primary rounded-bottom p-1">
        {/* catching errors in component for showing code of function to not crash whole app  */}
        <ErrorBoundary
          fallback={(
            <Alert>
              Error occurred while trying to display code.
            </Alert>
          )}
          onError={(error) => console.error(error)}
        >
          <DiffViewWrapper
            oldCode={oldCode}
            newCode={newCode}
            diff={diff}
            oldStart={specification.oldStart}
            newStart={specification.newStart}
            oldEnd={specification.oldEnd}
            newEnd={specification.newEnd}
            showDiff={specification.calling === undefined}
            linesToShow={specification.calling === undefined
              ? null
              : specification.calling}
          />
        </ErrorBoundary>
      </Row>
    </div>
  );
}

Code.propTypes = {
  specification: PropTypes.shape({
    oldSrc: PropTypes.string,
    newSrc: PropTypes.string,
    diff: PropTypes.string,
    oldStart: PropTypes.number,
    newStart: PropTypes.number,
    oldEnd: PropTypes.number,
    newEnd: PropTypes.number,
    calling: PropTypes.arrayOf(PropTypes.number),
    type: PropTypes.string.isRequired,
  }).isRequired,
  oldFolder: PropTypes.string.isRequired,
  newFolder: PropTypes.string.isRequired,
  getFile: PropTypes.func.isRequired,
};
