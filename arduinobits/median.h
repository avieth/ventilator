#include <stdint.h>

#define MEDIAN_FILTER_SIZE 15
/* Musut be MEDIAN_FILTER_SIZE / 2 */
#define MEDIAN_INDEX 7

struct median_16 {
  uint16_t value;
  uint8_t index;
  uint16_t observation_order[MEDIAN_FILTER_SIZE];
  uint16_t magnitude_order[MEDIAN_FILTER_SIZE];
};

void median_16_step(struct median_16 *mdata, uint16_t sample);
