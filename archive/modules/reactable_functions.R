aggregated.mean_coverage <- JS("
    function(cellInfo) {
      let values = cellInfo.subRows.map(row => row['mean_coverage']);
      let uniqueValues = [...new Set(values)];
      // Function to determine color based on value
      function getColor(value) {
        if (value >= .98) {
          return '#3A488AFF';
        } else if (value >= .97 & value < .98) {
          return '#8785B2FF';
        } else if (value >= .96 & value < .97) {
          return '#DABD61FF';
        } else if (value >= .95 & value < .96) {
          return '#D95F30FF';
        } else {
          return '#BE3428FF';
        }
      }
      // Format unique values as percentages with 1 decimal place and style them
      return uniqueValues
        .map(v => {
          let color = getColor(v);
          return `<span style='color: ${color}; font-weight: bold;'>${(v * 100).toFixed(1)}%</span>`;
        })
        .join('; ');
    
    } 
  ")

aggregated.depth <- JS("
    function(cellInfo) {
      let values = cellInfo.subRows.map(row => row['depth']);
      let uniqueValues = [...new Set(values)];

      // Function to determine color based on value
      function getColor(value) {
        if (value >= 10000) {
          return '#3A488AFF';
        } else if (value >= 7000 && value < 10000) {
          return '#8785B2FF';
        } else if (value >= 5000 && value < 7000) {
          return '#DABD61FF';
        } else if (value >= 1000 && value < 5000) {
          return '#D95F30FF';
        } else {
          return '#BE3428FF';
        }
      }

      // Format unique values with their respective colors
      return uniqueValues
        .map(v => {
          let color = getColor(v);
          return `<span style='color: ${color}; font-weight: bold;'>${Number(v).toLocaleString()}</span>`;
        })
        .join('; ');
    }
  ")

style.depth <- function(value) {
  if (value >= 10000) {
    color <- "#3A488AFF"
  } else if (value >= 7000 & value < 10000) {
    color <- "#8785B2FF"
  } else if (value >= 5000 & value < 7000) {
    color <- "#DABD61FF"
  } else if (value >= 1000 & value < 5000) {
    color <- "#D95F30FF"
  } else {
    color <- "#BE3428FF"
  }
  list(color = color, fontWeight = "bold")
}

style.mean_coverage <- function(value) {
  if (value >= .98) {
    color <- "#3A488AFF"
  } else if (value >= .97 & value < .98) {
    color <- "#8785B2FF"
  } else if (value >= .96 & value < .97) {
    color <- "#DABD61FF"
  } else if (value >= .95 & value < .96) {
    color <- "#D95F30FF"
  } else {
    color <- "#BE3428FF"
  }
  list(color = color, fontWeight = "bold")
}
