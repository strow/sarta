CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      This block of code is for use with AIRS RTP files to convert
C      prof.pnote into 17 modules yoffsets and doppler_ppm, and
C      then combine that with DFCAL to gebnerate NCHAN yoffsets.
       IF (PROF.pnote(1:8) .EQ. 'YOFF&DOP') THEN
C         Read 17 module yoffset (in um)
          DO II=1:17
             JJ = 5 + II*4
             YOFF(I) = C4TOR4( PROF.pnote(JJ:JJ+3) )
          ENDDO
C         Read Doppler_shift_ppm and convert ppm to um
          DOP = -0.12*C4TOR4( PROF.pnote(77:80) )
       ELSE
          DO II=1:17
             YOFF(II) = YOFFST
          ENDDO
          DOP = 0;
       ENDIF
       FOR II=1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
