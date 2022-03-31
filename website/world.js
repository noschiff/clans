class World {

  constructor(width, height) {
    this.width = width;
    this.height = height;

    this.cellArray = new Array(height);
    for (let i = 0; i < height; i++) {
      var widthArray = new Array(width);
      for (let j = 0; j < width; j++) {
        widthArray[j] = new Cell(i, j
          , "empty", 0, {});
      }

      this.cellArray[i] = widthArray;
    }
  }

  /**
   * Gets the specified cell at the specified coordinates.
   * @param {Number} x 
   * @param {Number} y 
   * @returns The cell at the coordinates, or null if the cell is out of bounds.
   */
  getCell(x, y) {
    if (!this.isInBounds(x, y)) {
      return null;
    }
    return this.cellArray[x][y];
  }

  setCell(x, y, cell) {
    this.cellArray[x][y] = cell;
  }

  /**
   * Gets the width of the world.
   * @returns The width.
   */
  getWidth() {
    return this.width;
  }

  /**
   * Gets the height of the world.
   * @returns The height
   */
  getHeight() {
    return this.height;
  }

  /**
   * Checks to see whether or not the specified coordinate is within the world bounds.
   * @param {Number} x 
   * @param {Number} y 
   * @returns True if within bounds, false otherwise.
   */
  isInBounds(x, y) {
    return x > -1 && y > -1 && x < world.getWidth() && y < world.getHeight();
  }

  // TODO: Create a method that loads in the world file from a JSON
}