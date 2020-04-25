#include "median.h"

void median_16_step(struct median_16 *mdata, uint16_t newest) {

  uint16_t oldest = mdata->observation_order[mdata->index];

  /* Replace the oldest sample in the observation order and bump the index. */
  mdata->observation_order[mdata->index] = newest;
  mdata->index = (mdata->index + 1) % MEDIAN_FILTER_SIZE;

  /* If the sample is the same as the oldest, then the magntide order has not
   * changed either and we're done
   */
  if (oldest == newest) {
    return;
  }

  /* Otherwise, update the magntiude order using a single pass over it.
   * 1. The oldest sample is somewhere in this list, and must be removed.
   * 2. The current sample must be placed within.
   * One of these is greater than the other. We traverse the list until we
   * find either one of them (or anything which is number-identical to them).
   * If we find the old sample first, we begin shifting later samples back,
   * until we find the proper spot for the new sample.
   *
   */
  int16_t i;
  if (oldest < newest) {
    for (i = 0; i < MEDIAN_FILTER_SIZE; i++) {
      /* Skip the prefix of the list that's less than oldest and newest */
      if (mdata->magnitude_order[i] < oldest) {
        continue;
      }
      /* Once we find something greater than newest we are done.
       * This cannot happen on i = 0 since oldest is in this sorted list,
       * and it's less than newest.
       */
      if (mdata->magnitude_order[i] > newest) {
        mdata->magnitude_order[i-1] = newest;
        break;
      }
      /* Shift one spot to the left anything which is greater than oldest.
       * This cannot happen on i = 0 since oldest is in this sorted list.
       */
      if (mdata->magnitude_order[i] > oldest) {
        mdata->magnitude_order[i-1] = mdata->magnitude_order[i];
      }
    }
    /* Check whether anything in the array was greater than the newest value.
     * If there is something greater than newest, then the loop was broken
     * prematurely, so i did not reach its upper bound.
     */
    if (i == MEDIAN_FILTER_SIZE) {
      mdata->magnitude_order[MEDIAN_FILTER_SIZE - 1] = newest;
    }
  } else {
    /* This is basically the same as for the previous case, but we go from
     * highest-to-lowest, shifting right once the oldest is found. Think of
     * it this way: it's the same thing but with the order notion flipped
     * around (a < b becomes a > b).
     */
    for (i = (MEDIAN_FILTER_SIZE - 1); i >= 0; i--) {
      if (mdata->magnitude_order[i] > oldest) {
        continue;
      }
      if (mdata->magnitude_order[i] < newest) {
        mdata->magnitude_order[i+1] = newest;
        break;
      }
      if (mdata->magnitude_order[i] < oldest) {
        mdata->magnitude_order[i+1] = mdata->magnitude_order[i];
      }
    }
    if (i == -1) {
      mdata->magnitude_order[0] = newest;
    }
  }

  mdata->value = mdata->magnitude_order[MEDIAN_INDEX];
}
