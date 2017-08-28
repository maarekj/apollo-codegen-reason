const webpack = require('webpack');
const path = require('path');

module.exports = {
  target: "node",
  entry: {
    cli: './lib/js/src/acr_cli.js',
  },
  output: {
    path: path.join(__dirname, "lib/output"),
    filename: '[name].js',
  },
  plugins: [
    new webpack.BannerPlugin({
      banner: "#!/usr/bin/env node",
      raw: true,
      entryOnly: true,
    }),
  ],
};
