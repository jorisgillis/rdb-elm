var gulp = require('gulp');
var preprocess = require('gulp-preprocess');
var process = require('process');
var cmd = require('gulp-run-command').default;

gulp.task('environment variables', function () {
  gulp.src('./html/*.html')
    .pipe(preprocess({context: { CLIENT_ID: process.env['RDB_CLIENT_ID'], DEBUG: true}}))
    .pipe(gulp.dest('./dist/'))
});

gulp.task('copy App.js', function () {
  gulp.src('./App.js')
    .pipe(gulp.dest('./dist/'))
});

gulp.task('copy css', function () {
  gulp.src('./*.css')
    .pipe(gulp.dest('./dist/'))
});

gulp.task('compile Elm', function () {
  cmd('elm-make Main.elm --output App.js --debug', {})
});

gulp.task('default', [
  'compile Elm',
  'environment variables',
  'copy App.js',
  'copy css'
])
