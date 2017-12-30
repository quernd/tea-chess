import node_resolve from 'rollup-plugin-node-resolve';
import livereload from 'rollup-plugin-livereload';
import uglify from 'rollup-plugin-uglify';

export default {
    input: './src/Main.bs.js',
    output: {
        file: './release/main.js',
        format: 'iife'
    },
    plugins: process.env.BUILD == "demo" ? [
        node_resolve({module: true, browser: true})
    ] : [
        node_resolve({module: true, browser: true}),
        livereload('release')
    ],
    name: 'starter',
    watch: {
        clearScreen: false
    }
}
