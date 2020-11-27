const path = require("path");

module.exports = {
    mode: "development",
    devtool: "eval-source-map",
    entry: "./src/main.ts",
    module: {
        rules: [
            {
                test: /\.ts?$/,
                use: "ts-loader",
                include: [path.resolve(__dirname, "src")]
            }
        ]
    },
    resolve: {
        extensions: [".ts", ".js"]
    },
    devServer: {
        contentBase: path.join(__dirname, 'dist'),
        compress: true,
        port: 8100
    },
    output: {
        publicPath: "dist",
        filename: "bundle.js",
        path: path.resolve(__dirname, "dist")
    }
}