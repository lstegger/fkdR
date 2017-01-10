module.exports = function(grunt) {
    grunt.initConfig({
        pdflatex: {
            options: {
                executable: "pdflatex"
            },
            thesis: {
                files: [{
                    src: "00_thesis.tex"
                }]
            }
        },
        watch: {
            thesis: {
                files: ['*.tex', '*.cls'],
                tasks: [
					'pdflatex:thesis',
				    'pdflatex:thesis',
					'glossaries:thesis',
					'bibtex:thesis',
					'pdflatex:thesis',
				    'pdflatex:thesis',
					'wordcount:thesis'
				],
                options: {
                    spawn: false,
                },
            }
        },
		glossaries: {
		    options: {
		      executable: "makeglossaries" /* might have to be changed for your system */
		    },
			thesis: {
                files: [{
                    src: ['*.glo', '*.acn']
                }]
            }
		},
		wordcount: {
		    thesis: {
		      files: [{
				src: '*.tex'
			  }]
		    }
		},
		bibtex: {
		    options: {
		      executable: "bibtex"
		    },
		    thesis: {
                files: [{
                    src: "00_thesis.aux"
                }]
            }
		}
    });

    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-tex-pdflatex');
	grunt.loadNpmTasks('grunt-tex-glossaries');
	grunt.loadNpmTasks('grunt-tex-bibtex');
	grunt.loadNpmTasks('grunt-tex-wordcount');

    grunt.registerTask('build', [
	    'pdflatex:thesis',
	    'pdflatex:thesis',
		'glossaries:thesis',
		'bibtex:thesis',
		'pdflatex:thesis',
	    'pdflatex:thesis',
		'wordcount:thesis'
    ]);

    grunt.registerTask('default', [
	    'pdflatex:thesis',
	    'pdflatex:thesis',
		'glossaries:thesis',
		'bibtex:thesis',
		'pdflatex:thesis',
	    'pdflatex:thesis',
		'wordcount:thesis',
		'watch:thesis'
    ]);
};