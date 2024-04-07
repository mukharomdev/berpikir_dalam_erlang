# Istilah Yang Harus Dipahami Programmer Erlang

Erlang adalah bahasa pemrograman yang dirancang untuk sistem berskala besar dan real-time yang membutuhkan tingkat kesalahan yang sangat rendah, seperti telekomunikasi, perbankan, e-commerce, dan sistem pesan instan. Sebagai seorang programmer Erlang, ada beberapa istilah penting yang perlu Anda pahami untuk bisa mengembangkan aplikasi dengan efektif:

    Actor Model: Erlang menggunakan model aktor untuk konkurensi. Dalam model ini, aktor adalah entitas yang melakukan komputasi, menyimpan state, dan dapat berkomunikasi dengan aktor lain melalui pengiriman pesan.

    Process: Dalam konteks Erlang, proses merujuk pada thread eksekusi yang sangat ringan dan terisolasi. Proses-proses ini berkomunikasi satu sama lain melalui pengiriman pesan dan dijalankan secara bersamaan pada mesin virtual Erlang (BEAM).

    BEAM: Erlang VM (Mesin Virtual Erlang) yang menjalankan bytecode Erlang. BEAM dirancang untuk sistem real-time dengan dukungan yang kuat untuk konkurensi, distribusi, dan fault tolerance.

    OTP (Open Telecom Platform): Kumpulan middleware, perpustakaan, dan alat yang dirancang untuk mendukung pengembangan aplikasi yang kuat dengan Erlang. OTP menyediakan kerangka kerja untuk aplikasi Erlang, termasuk desain pola yang teruji, implementasi server, sistem pemantauan, dan lain-lain.

    Supervisor Trees: Struktur hierarki dari proses yang digunakan untuk membangun aplikasi fault-tolerant. Supervisor bertanggung jawab untuk memulai, menghentikan, dan memonitor proses anaknya, dan mengambil tindakan ketika ada yang gagal.

    Pattern Matching: Teknik yang digunakan untuk memeriksa struktur data dengan pola tertentu. Erlang memanfaatkan pattern matching secara luas, baik dalam fungsi parameter maupun case statements, untuk memudahkan penanganan berbagai kondisi.

    Hot Code Swapping: Kemampuan untuk mengganti kode yang sedang berjalan dengan versi baru tanpa perlu menghentikan atau mengganggu sistem. Ini memungkinkan pembaruan pada aplikasi real-time tanpa downtime.

    Erlang Shell: Lingkungan interaktif yang digunakan untuk mengembangkan dan menguji kode Erlang secara dinamis. Erlang Shell sangat berguna untuk prototyping cepat dan debugging.

    ETS (Erlang Term Storage): Sistem penyimpanan data in-memory yang memungkinkan proses Erlang menyimpan dan mengambil data dengan cepat. ETS mendukung akses konkuren dan dapat digunakan untuk menyimpan state yang dibagi antar proses.

    Mnesia: Sistem database terdistribusi yang dibangun di atas Erlang. Mnesia dirancang untuk aplikasi soft real-time dan mendukung transaksi, replikasi, dan tabel yang dapat disimpan baik di disk maupun di memori.

Memahami istilah-istilah ini akan membantu Anda dalam merancang, mengembangkan, dan memelihara aplikasi Erlang dengan lebih efektif, memanfaatkan keunikan dan kekuatan yang ditawarkan oleh bahasa dan ekosistemnya.