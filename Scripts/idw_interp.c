#include <math.h>
#include <stdio.h>

// Function to performs IDW interpolation (distance calculation and interpolation logic in one function)
void idw_interpolation(int *num_points,         // Number of points
                       double *pollutant,      // Pointer to pollutant values (size: num_points)
                       double *dist,           // Pointer to distances (size: num_points)
                       double *p,               // Power parameter for IDW
                       double *interpolated) { // Output interpolated value
  
  double numerator = 0.0;
  double denominator = 0.0;
  int distance_exact = 0;
  double threshold = 0.0001;  // Threshold for "exact match"
  int i;
  
  // Loop through all points to check if any distance is "close enough" to zero (exact match)
  for (i = 0; i < *num_points; i++) {
    if (dist[i] < threshold) {
      *interpolated = pollutant[i];  // Return the exact value if the query point is the same as a data point
      distance_exact = 1;
      printf("Exact match found at index %d, value: %lf\n", i, pollutant[i]);
      break;  // No need to continue the loop once we find an exact match
    }
  }
  
  // If there was no exact match, proceed with the interpolation
  if (distance_exact == 0) {
    // Loop through all points to calculate the weighted sum of values and the sum of weights
    for (i = 0; i < *num_points; i++) {
      // Check for very small distances that may cause instability
      if (dist[i] < threshold) {
        printf("Warning: Very small distance at index %d, dist: %lf\n", i, dist[i]);
      }
      
      if (dist[i] > 0.0) {  // Only consider points with non-zero distance
        double w_i = 1.0 / pow(dist[i], *p);  // Calculate the weight based on the inverse distance raised to the power p
        numerator += pollutant[i] * w_i;     // Accumulate the weighted value
        denominator += w_i;                  // Accumulate the sum of weights
        
        // Debug: Print the calculated weight
        // printf("Weight for point %d (dist: %lf, w_i: %lf)\n", i, dist[i], w_i);
      } else {
        printf("Warning: Zero distance at index %d, dist: %lf\n", i, dist[i]);
      }
    }
    
    // Debug: Check the numerator and denominator values before division
    // printf("Numerator: %lf, Denominator: %lf\n", numerator, denominator);
    
    // Ensure denominator is non-zero to avoid division by zero
    if (denominator > 0.0) {
      *interpolated = numerator / denominator;  // Calculate the interpolated value
    } else {
      // Handle edge case where all distances are zero
      *interpolated = 0.0;  // Or some default value/error handling
      printf("Error: Denominator is zero, cannot compute interpolation.\n");
    }
  }
}


