(lang dune 2.8)

;; Copyright 2021/2022 Johns Hopkins University Applied Physics Laboratory

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(generate_opam_files true)
(name openapi)

(package
 (name openapi)
 (version 1.0.0)
 (synopsis "Openapi documentation infrastructure")
 (description "Core types and functions for creating openapi compliant documentation using JSON schemas")
 (license "BSD-3-Clause")
 (authors "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (maintainers "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (homepage "https://github.com/jhuapl-saralab/openapi-ocaml")
 (source (github "jhuapl-saralab/openapi-ocaml"))
 (bug_reports "https://github.com/jhuapl-saralab/openapi-ocaml/issues")
 (depends
  (yojson (>= 2.1.2))
  (core (>= v0.16.0))
  (ppx_yojson_conv (>= v0.16.0))
  (ppx_yojson_conv_lib (>= v0.16.0))
  (ppx_deriving (>= 5.2.1))))

(package
 (name openapi-opium)
 (version 1.0.0)
 (synopsis "Openapi documentation generation for Opium")
 (description "Shim layer around Opium.App (https://github.com/rgrinberg/opium/) for producing Openapi documentation pages")
 (license "BSD-3-Clause")
 (authors "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (maintainers "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (homepage "https://github.com/jhuapl-saralab/openapi-ocaml")
 (source (github "jhuapl-saralab/openapi-ocaml"))
 (bug_reports "https://github.com/jhuapl-saralab/openapi-ocaml/issues")
 (depends
  (openapi (= 1.0.0))
  (opium (>= 0.20.0))))

(package
 (name ppx_deriving_json_schema)
 (version 1.0.0)
 (synopsis "Ppx deriver for generating JSON schema objects from type definitions")
 (license "BSD-3-Clause")
 (authors "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (maintainers "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>")
 (homepage "https://github.com/jhuapl-saralab/openapi-ocaml")
 (source (github "jhuapl-saralab/openapi-ocaml"))
 (bug_reports "https://github.com/jhuapl-saralab/openapi-ocaml/issues")
 (depends
  (ppxlib (>= 0.31.0))
  (core (>= v0.16.0))
  (openapi (= 1.0.0))))
